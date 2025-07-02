(ns gent-clj.core
  (:require [clj-http.client :as client]
            [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]))
(json/write-str {:model "openai/gpt-4.1",
                 :messages [{:role "user",
                             :content "What is the capital of France?"}]})

;; curl -L \
  ;; -X POST \
  ;; -H "Accept: application/vnd.github+json" \
  ;; -H "Authorization: Bearer <YOUR-TOKEN>" \
  ;; -H "X-GitHub-Api-Version: 2022-11-28" \
  ;; -H "Content-Type: application/json" \
  ;; https://models.github.ai/orgs/ORG/inference/chat/completions \
  ;; -d '{"model":"openai/gpt-4.1","messages":[{"role":"user","content":"What
  ;; is
  ;; the capital of France?"}]}'

(def api-base (System/getenv "OPENAI_API_BASE"))
(def api-key (System/getenv "OPENAI_API_KEY"))

(def function-declarations
  [{:name "git-ls-files",
    :description "Lists files in a git repository.",
    :definition (fn [] (throw (Exception. "Not implemented: git-ls-files")))}
   {:name "open-git-file",
    :description "Lists files in a git repository.",
    :parameters {:type "object",
                 :properties {:path
                                {:type "string",
                                 :description
                                   "The path to the file in the repository."}},
                 :required ["path"]},
    :definition (fn [] (throw (Exception. "Not implemented: open-git-file")))}])

(def roles {:user "user", :model "model"})
(defn call-api
  [contents]
  (->
    (client/post
      "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent"
      {:headers {"x-goog-api-key" (System/getenv "GEMINI_API_KEY"),
                 "Content-Type" "application/json"},
       :throw-exceptions false,
       :body (json/write-str {:contents contents,
                              :tools [{:functionDeclarations
                                         (map #(dissoc % :definition)
                                           function-declarations)}]})})
    :body
    (json/read-str)
    (get-in ["candidates" 0 "content"])))

(comment
  (call-api [{:role "user",
              :parts [{:text "list the files in the current git repo"}]}]))

;; {"parts" [{"functionCall" {"name" "git-ls-files", "args" {}}}], "role"
;; "model"}

(:definition (first (filter #(= (get-in {"functionCall" {"name" "git-ls-files"}}
                                        ["functionCall" "name"])
                                (:name %))
                      function-declarations)))

(defn handle-gemini-response
  [response]
  (map (fn [part]
         {:role "user",
          :functionResponse
            (let* [function-call (get part "functionCall") function-args
                   (get function-call "args") function-name
                   (get function-call "name") found-fn
                   (:definition (first (filter #(= function-name (:name %))
                                         function-declarations))) fn-result
                   (if (found-fn)
                     (found-fn function-args)
                     {:error (str "Function not found: " function-name)})]
                  fn-result)})
    (get response "parts")))

(handle-gemini-response {"parts" [{"functionCall" {"name" "git-ls-files"}}]})


(defn run-prompt
  [prompt]
  (loop [iters-left 5
         contents [{:role "user", :parts [{:text prompt}]}]]
    (let* [gemini-response (call-api contents)]
          (if (> iters-left 0)
            (recur (dec iters-left) (conj contents gemini-response))
            contents))))
(run-prompt "list the files")

(comment
  (-> (client/get (str api-base "/models")
                  {:headers {"Authorization" (str "Bearer " api-key),
                             "Accept" "application/json",
                             "Content-Type" "application/json"},
                   :throw-exceptions false})
      :body
      json/read-str
      (get "data")
      ;; #(map (fn [x] (get-in x ["capabilities" "family"]) %1))
  ))
(comment
  (json/read-str
    (:body (client/post
             (str api-base "/chat/completions")
             {:headers {"Authorization" (str "Bearer " api-key),
                        "Accept" "application/json",
                        "Content-Type" "application/json"},
              :body
                (json/write-str
                  {:model "gpt-3.5-turbo",
                   :tools [],
                   :messages
                     [{:role "user",
                       :content
                         "What isp
              the capital of France?"}]}),
              :throw-exceptions false}))))

(comment
  (json/read-str
    (:body (client/post
             (str api-base "/chat/completions")
             {:headers {"Authorization" (str "Bearer " api-key),
                        "Accept" "application/json",
                        "Content-Type" "application/json"},
              :body
                (json/write-str
                  {:model "gpt-3.5-turbo",
                   :messages
                     [{:role "user",
                       :content
                         "What isp
              the capital of France?"}]}),
              :throw-exceptions false}))))
(defn foo "I don't do a whole lot." [x] x)

(deftest another-test (testing "FIXME2, I fail." (is (= "hi" (foo "hi")))))
