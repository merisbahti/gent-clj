(ns gent-clj.core
  (:require [clj-http.client :as client]
            [clojure.data.json :as json]
            [clojure.java.shell :as sh]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [malli.core :as m]))

(def api-base (System/getenv "OPENAI_API_BASE"))
(def api-key (System/getenv "OPENAI_API_KEY"))

(defn git-ls-files
  []
  (-> (sh/sh "git" "ls-files")
    :out
    (str/split #"\n")))
(deftest ls-files
  (is (every? string? (git-ls-files)))
  (is (some (partial = "README.md") (git-ls-files))))

(defrecord FunctionDeclaration [name description parameters definition])
(deftype EndOfConversation [])

(def function-declarations
  [(->FunctionDeclaration
     "end"
     "Call when there's nothing else to do"
     {:type "object",
      :properties {:message
                   {:type "string",
                    :description
                    "A message to send when ending the conversation."}},
      :required ["message"]}
     (fn [& _] EndOfConversation))
   (->FunctionDeclaration "git-ls-files"
     "Lists files in a git repository."
     nil
     (fn [& _] (git-ls-files)))
   (->FunctionDeclaration
     "open-file"
     "Show contents of a file in the git repository."
     {:type "object",
      :properties {:path {:type "string",
                          :description
                          "The path to the file in the repository."}},
      :required ["path"]}
     (fn [{path :path}]
       (if (some (partial = path) (git-ls-files))
         (try {:content (slurp path)}
           (catch Exception _ {:error (str "Error reading file: " path)}))
         {:error (str "File not in git path: " path)})))])

(def ApiResponse [:map ["candidates" [:sequential [:map ["content" [:map]]]]]])

(def modelnames {:gemini-2.5-flash-lite "gemini-2.5-flash-lite-preview-06-17"})

(defn call-api
  [contents]
  (->
    (client/post
      (format
        "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent"
        (:gemini-2.5-flash-lite modelnames))
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

(comment
  (run-prompt "list the names of the files this repo"))

(defn handle-gemini-response
  [response]
  (map (fn [{{function-name :name, function-args :args} :functionCall}]
         (println function-name)
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
    (:parts response)))

(deftest handle-gemini-response-test
  (is (= (handle-gemini-response {:parts []}) []))
  (is (= (handle-gemini-response {:parts [{:functionCall {:name "end",
                                                          :args ""}}]})

        (list {:parts [{:functionResponse
                        {:name "end",
                         :response {:result
                                    gent_clj.core.EndOfConversation}}}],
               :role "user"}))))

(defn should-end?
  [{parts :parts}]
  (some (comp (partial = "end") :name :functionCall) parts))
(deftest should-end-test
  (testing "should end"
    (is (= true
          (should-end? {:role "model",
                        :parts [{:functionCall {:name "end", :args {}}}]}))))
  (testing "keep going?"
    (is (not (= true
               (should-end? {:parts [{:functionCall {:name "stuff",
                                                     :args {}}}],
                             :role "model"}))))))
(defn run-prompt
  [prompt]
  (loop [iters-left 5
         contents [{:role "user", :parts [{:text prompt}]}]]
    (let*
      [gemini-response         (call-api contents)
       handled-gemini-response (handle-gemini-response gemini-response)
       should-end              (should-end? gemini-response)
       new-contents            (concat contents [gemini-response] handled-gemini-response)]
      (if (and (> iters-left 0) (not should-end))
        (recur (dec iters-left) new-contents)
        new-contents))))

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

(defn -main [xs] ((comp dorun map) pprint/pprint (run-prompt xs)))
