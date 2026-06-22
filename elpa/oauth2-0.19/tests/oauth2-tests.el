;;; oauth2-tests.el --- oauth2.el tests -*- lexical-binding: t -*-

(require 'oauth2)
(require 'ert)

(ert-deftest oauth2--build-url-param-str-test ()
  (should (string=
           (oauth2--build-url-param-str "simple" "plain"
                                        "empty" nil
                                        "empty2" ""
                                        "email" "a@example.com")
           "simple=plain&email=a%40example.com"))
  (should (string=
           (oauth2--build-url-param-str "url" "http://localhost"
                                        "random" "12+3_4_=5=/6/")
           "url=http%3A%2F%2Flocalhost&random=12%2B3_4_%3D5%3D%2F6%2F"))
  (should-error (oauth2--build-url-param-str "novalue")
                :type 'error))

(ert-deftest oauth2--build-url-test ()
  (should (string=
           (oauth2--build-url "http://127.0.0.1"
                              "request=auth&login_hint=manphiz%40outlook.com")
           "http://127.0.0.1?request=auth&login_hint=manphiz%40outlook.com"))
  (should (string=
           (oauth2--build-url "https://localhost"
                              "simple" "plain"
                              "empty" nil
                              "complex" "1+2@3#4_5/6"
                              "empty2" "")
           "https://localhost?simple=plain&complex=1%2B2%403%234_5%2F6")))

(ert-deftest oauth2--generate-code-verifier-length-test ()
  ;; base64 encoding on a string of 90 results in 120.
  (should (=
           (length (oauth2--generate-code-verifier 90))
           120)))

(ert-deftest oauth2--get-challenge-from-verifier-test ()
  ;; Using pre-generated code-verifier values from mutt_oauth2.py for testing.
  (let ((test-cases
         '((:verifier
            "nDe_cq5hGQC6-_OUhE4Y3jVdrPmRVvzSRuNci4efeXeHBiGSqAmVbzMioNMwD1fQn96IL2mChFBzhv2kI02kHNTU1tHI2T9tWn5_Lp9rqy3fGR90WYxYXGKz"
            :challenge "hqvORBgWMedJHg2HnNs7DcRjEnVuk7gGQi9iBcp7PRs")
           (:verifier
            "WItNqcP9W_HFOZV__P5FgYKlbkTOBolU0jWMMIiTTh6rcG3TyoRtV4Ozx7nIJhowhjAjt41gmHwuKgxGhtv1k_5XDj52udYwHdSgqUrmkvhaqYgLADAp7rrf"
            :challenge "lB2AKQFg6caqfa3u0cnxXihnU69vvGG1cUPRi8_cvpE")))
        (expected-challenge-length 43))
    (dolist (test-case test-cases)
      (let* ((verifier (plist-get test-case :verifier))
             (challenge (oauth2--get-challenge-from-verifier verifier))
             (expected-challenge (plist-get test-case :challenge)))
        (should (string= challenge expected-challenge))
        (should (= (length challenge) expected-challenge-length))))))

(ert-deftest oauth2--build-authorization-request-url-test ()
  (should (string=
           (oauth2--build-authorization-request-url
            "https://authrization.url/request-token"
            "random_client_id"
            "https://localhost:5678/"
            "email_scope"
            "random_state"
            "test@example.com"
            "random_hash_of_verifier")
           "https://authrization.url/request-token?client_id=random_client_id&response_type=code&redirect_uri=https%3A%2F%2Flocalhost%3A5678%2F&scope=email_scope&state=random_state&login_hint=test%40example.com&access_type=offline&prompt=consent&code_challenge=-MXasMXxsnGVodkChYqEubsH0BnyQlhudAhfjqpyeZA&code_challenge_method=S256")))

(ert-deftest oauth2--handle-encoded-authorization-code ()
  (should (string=
           (oauth2--handle-encoded-code
            "M.C543_SN1.2.U.DjYxUvMWVxeks6k*o39EISowXUKV8owObY4Fwb8Ivle7PkLriFpNzxJX4nDna2ub4bPQP71LOZndyOZkN8SAxj2bfZ3r9y6NpdluQSA*7GPcDvQ6b9BQgfJgCRf!WrTrlZsGR6wqX3bTWBFZVDeNvX!rDDTUVGWaRjyRcZCZtrwzdmXovPV8BsjiAEZRGvxKLUFcpKlwy*EepJ*9w7p9R06714EbMqgLNzdEku5aAxJqMVgyrANroJBsutapE!0GYzdN6BS!3BLWY1CUskBLFL1fKGYc0*jJL6!zYKU2nyvr*3RGPlCcFGOmfcP1ptVHKio0bRRQDpvR0cmxJJg8PVWXtipJDvHKZsgOLQFDYnNnEkTzbpQ0o5K9DWgPXc8nWmPi8ZED5j*WSekCPpkyIRdcGfsNb5cJYhRPcA2TuQuZVRegdduVzjjqAotQGFLHZkziUK*I10bGmt8QedIwgVc8GNBvNC7D6EBcF2ytfPSxF477Ke9wPm9TpkzTRkC!MvPGDE4oaLu4siv2Rrq96kK!yme!od8kcxMBadM2PLe06DzkDeH42BhlCDzhnGuqhqW!VHxmQhBVlMop6Z*N8DnV3Bw6knttia21lHQOaMc5sBvE2noA9HiEe9vSKEnMIgeD9AeXDq83686fPkiALE%24")
           "M.C543_SN1.2.U.DjYxUvMWVxeks6k*o39EISowXUKV8owObY4Fwb8Ivle7PkLriFpNzxJX4nDna2ub4bPQP71LOZndyOZkN8SAxj2bfZ3r9y6NpdluQSA*7GPcDvQ6b9BQgfJgCRf!WrTrlZsGR6wqX3bTWBFZVDeNvX!rDDTUVGWaRjyRcZCZtrwzdmXovPV8BsjiAEZRGvxKLUFcpKlwy*EepJ*9w7p9R06714EbMqgLNzdEku5aAxJqMVgyrANroJBsutapE!0GYzdN6BS!3BLWY1CUskBLFL1fKGYc0*jJL6!zYKU2nyvr*3RGPlCcFGOmfcP1ptVHKio0bRRQDpvR0cmxJJg8PVWXtipJDvHKZsgOLQFDYnNnEkTzbpQ0o5K9DWgPXc8nWmPi8ZED5j*WSekCPpkyIRdcGfsNb5cJYhRPcA2TuQuZVRegdduVzjjqAotQGFLHZkziUK*I10bGmt8QedIwgVc8GNBvNC7D6EBcF2ytfPSxF477Ke9wPm9TpkzTRkC!MvPGDE4oaLu4siv2Rrq96kK!yme!od8kcxMBadM2PLe06DzkDeH42BhlCDzhnGuqhqW!VHxmQhBVlMop6Z*N8DnV3Bw6knttia21lHQOaMc5sBvE2noA9HiEe9vSKEnMIgeD9AeXDq83686fPkiALE$")))

;;; oauth2-tests.el ends here.
