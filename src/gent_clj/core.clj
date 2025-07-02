(ns gent-clj.core
  (:require [clj-http.client :as client]
            [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.walk :as walk]
            [clojure.java.shell :as sh]
            [clojure.test :refer [deftest is testing]]
            [malli.core :as m]))
(json/write-str {:model "openai/gpt-4.1",
                 :messages [{:role "user",
                             :content "What is the capital of France?"}]})


(def api-base (System/getenv "OPENAI_API_BASE"))
(def api-key (System/getenv "OPENAI_API_KEY"))
(defn git-ls-files [] (sh/sh "git" "ls-files"))

(defrecord FunctionDeclaration [name description parameters definition])
(deftype EndOfConversation [])



(def function-declarations
  [(->FunctionDeclaration "end"
                          "Call when there's nothing else to do"
                          nil
                          (fn [& _] EndOfConversation))
   (->FunctionDeclaration "git-ls-files"
                          "Lists files in a git repository."
                          nil
                          (fn [& _] (git-ls-files)))
   ;; (->FunctionDeclaration
   ;;   "open-file" "Show contents of a file in the git repository."
   ;;   {:type "object",
   ;;    :properties {:path {:type "string",
   ;;                        :description
   ;;                          "The path to the file in the repository."}},
   ;;    :required ["path"]}
   ;;   (fn [& _] (throw (Exception. "Not implemented: open-git-file"))))
  ])

(def roles {:user "user", :model "model"})

(def ApiResponse [:map ["candidates" [:sequential [:map ["content" [:map]]]]]])

(defn call-api
  [contents]
  (->
    (client/post
      "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash-lite-preview-06-17:generateContent"
      {:headers {"x-goog-api-key" (System/getenv "GEMINI_API_KEY"),
                 "Content-Type" "application/json"},
       :throw-exceptions false,
       :body (json/write-str
               {:contents contents,
                :tools [{:functionDeclarations (map #(dissoc % :definition)
                                                 function-declarations)}],
                :toolConfig {:functionCallingConfig {:mode "any"}}})})
    :body
    (json/read-str)
    (#(m/assert ApiResponse %))
    (get-in ["candidates" 0 "content"])
    (walk/keywordize-keys)))

(let [{{name :name, args :args} :functionCall}
        {:functionCall {:name "myname", :args {:arg "myarg"}}}]
  [name args])

(defn handle-gemini-response
  [response]
  (map (fn [{{function-name :name, function-args :args} :functionCall}]
         (let* [found-fn
                (:definition (first (filter #(= function-name (:name %))
                                      function-declarations)))]
               {:role "user",
                :parts [{:functionResponse
                           {:name (or function-name "not-found"),
                            :response {:result (if found-fn
                                                 (found-fn function-args)
                                                 {:error
                                                    (str "Function not found: "
                                                         function-name)})}}}]}))
    (get response :parts)))



(defn should-end?
  [parts]
  (-> parts
      (get :parts)
      (#(some (fn [part] (= "end" (get-in part [:functionCall :name]))) %))))
(deftest should-end-test
  (testing "should end"
    (is (= true
           (should-end? {:parts [{:functionCall {:name "end", :args {}}}],
                         :role "model"}))))
  (testing "keep going?"
    (is (not (= true
                (should-end? {:parts [{:functionCall {:name "stuff",
                                                      :args {}}}],
                              :role "model"}))))))

(defn run-prompt
  [prompt]
  (loop [iters-left 5
         contents [{:role "user", :parts [{:text prompt}]}]]
    (let* [gemini-response (call-api contents) handled-gemini-response
           (handle-gemini-response gemini-response) should-end
           (should-end? gemini-response) new-contents
           (concat contents [gemini-response] handled-gemini-response)]
          (println {:resp contents})
          (if (and (> iters-left 0) (not should-end))
            (recur (dec iters-left) new-contents)
            new-contents))))
(comment
  (run-prompt "list the files"))

(comment
  (-> (client/get (str api-base "/models")
                  {:headers {"Authorization" (str "Bearer " api-key),
                             "Accept" "application/json",
                             "Content-Type" "application/json"},
                   :throw-exceptions false})
      :body
      json/read-str
      walk/keywordize-keys
      :data))
      ;; #(map (fn [x] (get-in x ["capabilities" "family"]) %1))
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
