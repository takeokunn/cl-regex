# Testing Strategy: TDD with Property-Based Testing

## Overview

CL-Regex employs a comprehensive testing strategy combining:
- **Test-Driven Development (TDD)** for feature development
- **Property-Based Testing (PBT)** for exhaustive validation
- **Prolog-based specification testing** for formal verification

## Test Architecture

```
┌─────────────────────────────────────────────┐
│              Test Suite Architecture              │
├───────────────┬──────────────┬────────────────┤
│  Unit Tests   │ Integration  │ Property Tests  │
│               │    Tests     │                 │
├───────────────┴──────────────┴────────────────┤
│            Specification Tests                   │
│         (Prolog-based verification)              │
├───────────────────────┬──────────────────────┤
│   Performance Tests    │   Security Tests     │
│     & Benchmarks       │   & Fuzzing          │
└───────────────────────┴──────────────────────┘
```

## Test-Driven Development (TDD)

### TDD Cycle

```lisp
;; Step 1: Write failing test
(deftest test-email-pattern ()
  (let ((pattern (compile-regex email-pattern)))
    (assert-true (match pattern "user@example.com"))
    (assert-false (match pattern "invalid.email"))
    (assert-false (match pattern "@example.com"))
    (assert-false (match pattern "user@"))))

;; Step 2: Implement minimal code
(defpattern email-pattern
  (regex
    (+ (or alphanumeric "._%+-"))
    "@"
    (+ (or alphanumeric ".-"))
    "."
    (repeat 2 4 alpha)))

;; Step 3: Refactor
(defpattern email-pattern
  (regex
    (group 'local (+ (or alphanumeric "._%+-")))
    "@"
    (group 'domain (+ (or alphanumeric ".-")))
    "."
    (group 'tld (repeat 2 4 alpha))))
```

### Unit Test Structure

```lisp
(defsuite regex-unit-tests
  "Core functionality tests")

(deftest test-basic-matching (:suite regex-unit-tests)
  (testing "Literal matching"
    (assert-match "hello" "hello world")
    (assert-no-match "goodbye" "hello world"))
  
  (testing "Character classes"
    (assert-match "[a-z]+" "hello")
    (assert-match "\\d+" "12345")
    (assert-match "\\w+" "hello_world_123"))
  
  (testing "Quantifiers"
    (assert-match "a*" "")
    (assert-match "a+" "aaa")
    (assert-match "a?" "a")
    (assert-match "a{3}" "aaa")
    (assert-no-match "a{3}" "aa")))
```

## Property-Based Testing (PBT)

### Core Properties

```lisp
(defproperty pattern-matches-itself
  (for-all ((pattern (gen-valid-pattern)))
    (let ((text (generate-matching-text pattern)))
      (is (match pattern text)))))

(defproperty pattern-consistency
  (for-all ((pattern (gen-pattern))
            (text (gen-string)))
    (let ((result1 (match pattern text))
          (result2 (match pattern text)))
      (is (equal result1 result2)))))

(defproperty compilation-preserves-semantics
  (for-all ((pattern (gen-pattern-string)))
    (let ((compiled (compile-regex pattern))
          (interpreted (interpret-regex pattern)))
      (for-all ((text (gen-string)))
        (is (equal (match compiled text)
                   (match interpreted text)))))))
```

### Generator Functions

```lisp
(defgenerator gen-valid-pattern ()
  (one-of
    (gen-literal-pattern)
    (gen-character-class)
    (gen-quantified-pattern)
    (gen-alternation)
    (gen-grouped-pattern)))

(defgenerator gen-literal-pattern ()
  (map 'string
       (lambda (s) (escape-regex-chars s))
       (gen-string :min-length 1 :max-length 20)))

(defgenerator gen-character-class ()
  (format nil "[~A]"
          (apply #'concatenate 'string
                 (gen-list
                  (one-of
                    (gen-char-range)
                    (gen-single-char))))))

(defgenerator gen-quantified-pattern ()
  (let ((base (gen-simple-pattern))
        (quantifier (one-of "*" "+" "?" 
                           (format nil "{~D}" (gen-integer :min 1 :max 10))
                           (format nil "{~D,~D}" 
                                   (gen-integer :min 0 :max 5)
                                   (gen-integer :min 5 :max 10)))))
    (concatenate 'string base quantifier)))
```

### Property Specifications

```lisp
;; Algebraic Properties
(defproperty concatenation-associativity
  (for-all ((p1 (gen-pattern))
            (p2 (gen-pattern))
            (p3 (gen-pattern)))
    (pattern-equivalent
      (concat p1 (concat p2 p3))
      (concat (concat p1 p2) p3))))

(defproperty alternation-commutativity
  (for-all ((p1 (gen-pattern))
            (p2 (gen-pattern)))
    (pattern-equivalent
      (alt p1 p2)
      (alt p2 p1))))

;; Optimization Properties
(defproperty optimization-preserves-behavior
  (for-all ((pattern (gen-complex-pattern)))
    (let ((optimized (optimize-pattern pattern)))
      (for-all ((text (gen-test-string)))
        (is (equal (match pattern text)
                   (match optimized text)))))))
```

## Prolog-Based Specification Testing

### Formal Specifications

```lisp
;; Define pattern semantics in Prolog
(rule (matches-pattern ?pattern ?text ?result)
  (compile-pattern ?pattern ?compiled)
  (execute-match ?compiled ?text ?result))

(rule (valid-email ?text)
  (matches-pattern email-pattern ?text ?match)
  (not-null ?match)
  (extract-groups ?match ?groups)
  (valid-local-part (group-ref ?groups 'local))
  (valid-domain (group-ref ?groups 'domain))
  (valid-tld (group-ref ?groups 'tld)))

;; Test against specification
(deftest test-email-specification ()
  (for-all ((email (gen-email-string)))
    (let ((prolog-result (query (valid-email ?email)))
          (regex-result (match email-pattern email)))
      (is (equal (not (null prolog-result))
                 (not (null regex-result)))))))
```

### Invariant Checking

```lisp
(rule (pattern-invariant ?pattern)
  ;; No pattern matches empty and non-empty simultaneously
  (not (and (matches ?pattern "")
            (not (nullable ?pattern))))
  
  ;; Anchored patterns must start matching at position 0
  (implies (anchored ?pattern)
           (forall ?text
             (implies (matches ?pattern ?text)
                      (= (match-start ?pattern ?text) 0))))
  
  ;; DFA and NFA must produce same results
  (forall ?text
    (equal (match-with-nfa ?pattern ?text)
           (match-with-dfa ?pattern ?text))))

(deftest test-pattern-invariants ()
  (for-all ((pattern (gen-pattern)))
    (assert-true (query (pattern-invariant ?pattern)))))
```

## Test Categories

### Correctness Tests

```lisp
(defsuite correctness-tests
  "Verify correct pattern matching behavior")

(deftest test-unicode-support (:suite correctness-tests)
  (assert-match "\\p{L}+" "こんにちは")  ; Japanese
  (assert-match "\\p{L}+" "Привет")     ; Russian
  (assert-match "\\p{L}+" "😀😁")         ; Emoji
  (assert-match "\\p{Sc}" "€")            ; Currency symbols
  (assert-match "\\p{Nd}+" "१२३"))       ; Devanagari digits

(deftest test-backreferences (:suite correctness-tests)
  (assert-match "(\\w+)\\s+\\1" "hello hello")
  (assert-no-match "(\\w+)\\s+\\1" "hello world")
  (assert-match "<(\\w+)>.*?</\\1>" "<div>content</div>"))

(deftest test-lookaround (:suite correctness-tests)
  (assert-match "\\d+(?=\\$)" "100$" :expected "100")
  (assert-no-match "\\d+(?=\\$)" "100")
  (assert-match "(?<=\\$)\\d+" "$100" :expected "100")
  (assert-match "\\w+(?!\\d)" "hello world" :expected "hello"))
```

### Performance Tests

```lisp
(defsuite performance-tests
  "Verify performance characteristics")

(deftest test-compilation-performance (:suite performance-tests)
  (assert-performance
    (compile-regex "complex.*pattern.*with.*many.*parts")
    :max-time 0.1  ; 100ms
    :max-memory (* 10 1024 1024)))  ; 10MB

(deftest test-matching-performance (:suite performance-tests)
  (let ((pattern (compile-regex "[a-z]+@[a-z]+\\.[a-z]+"))
        (text (make-string 10000 :initial-element #\a)))
    (assert-performance
      (match pattern text)
      :max-time 0.01  ; 10ms
      :max-iterations 1000000)))  ; No catastrophic backtracking

(deftest test-catastrophic-backtracking (:suite performance-tests)
  ;; Known problematic patterns should fail gracefully
  (let ((pattern (compile-regex "(a+)+b"))
        (text (concatenate 'string 
                          (make-string 30 :initial-element #\a)
                          "c")))
    (assert-timeout
      (match pattern text)
      :timeout 1.0  ; Should timeout or optimize
      :expected-behavior :optimized)))
```

### Security Tests

```lisp
(defsuite security-tests
  "Verify resistance to attacks")

(deftest test-redos-protection (:suite security-tests)
  (for-all ((evil-pattern (gen-redos-pattern)))
    (let ((compiled (compile-regex evil-pattern :safety :high)))
      (assert-safe-execution
        (match compiled (generate-attack-string evil-pattern))
        :max-time 1.0
        :max-memory (* 100 1024 1024)))))

(deftest test-input-sanitization (:suite security-tests)
  (for-all ((untrusted (gen-untrusted-input)))
    (assert-no-error
      (compile-regex untrusted :validate t))))

(deftest test-memory-limits (:suite security-tests)
  (with-memory-limit (* 50 1024 1024)  ; 50MB limit
    (assert-signals memory-exhausted-error
      (compile-regex (generate-huge-pattern)))))
```

## Fuzzing

### Input Fuzzing

```lisp
(deffuzzer pattern-fuzzer
  :input-generator (gen-random-pattern)
  :oracle (lambda (pattern)
            (handler-case
                (let ((compiled (compile-regex pattern)))
                  (match compiled "test"))
              (regex-syntax-error () :invalid)
              (error (e) 
                (error "Unexpected error: ~A" e)))))

(deftest fuzz-test-patterns ()
  (run-fuzzer pattern-fuzzer
              :iterations 100000
              :timeout 3600))
```

### Differential Testing

```lisp
(deftest differential-testing ()
  (for-all ((pattern (gen-valid-pattern))
            (text (gen-string)))
    (let ((our-result (match pattern text))
          (reference-result (cl-ppcre:scan pattern text)))
      (assert-equivalent our-result reference-result))))
```

## Test Automation

### Continuous Integration

```yaml
# .github/workflows/test.yml
name: Test Suite
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Run Unit Tests
        run: |
          sbcl --load quicklisp.lisp \
               --eval '(ql:quickload :cl-regex-test)' \
               --eval '(run-tests :suite unit-tests)'
      - name: Run Property Tests
        run: |
          sbcl --eval '(run-property-tests :iterations 10000)'
      - name: Run Benchmarks
        run: |
          sbcl --eval '(run-benchmarks :compare-baseline t)'
```

### Test Coverage

```lisp
(defun measure-coverage ()
  (sb-cover:reset-coverage)
  (sb-cover:report-coverage
    (run-all-tests)
    :report-file "coverage.html"
    :form-mode :car))

;; Coverage goals:
;; - Line coverage: > 95%
;; - Branch coverage: > 90%
;; - Path coverage: > 85%
```

## Benchmark Suite

```lisp
(defbenchmark pattern-compilation
  :setup (lambda () (gen-pattern-string))
  :operation #'compile-regex
  :teardown #'clear-cache
  :iterations 10000)

(defbenchmark simple-matching
  :setup (lambda () 
           (list (compile-regex "[a-z]+")
                 "hello world"))
  :operation (lambda (pattern text) (match pattern text))
  :iterations 1000000)

(defbenchmark complex-matching
  :setup (lambda ()
           (list (compile-regex complex-email-pattern)
                 (generate-email)))
  :operation (lambda (pattern text) (match pattern text))
  :iterations 100000)

;; Run and compare
(compare-benchmarks
  '(pattern-compilation simple-matching complex-matching)
  :baseline "v1.0.0"
  :current "HEAD")
```

## Test Data Management

```lisp
(defparameter *test-corpus*
  (load-test-data "tests/data/corpus.lisp"))

(defstruct test-case
  pattern
  input
  expected-match
  expected-groups
  description)

(defun load-test-cases (file)
  (with-open-file (stream file)
    (loop for case = (read stream nil)
          while case
          collect (apply #'make-test-case case))))

;; Corpus-based testing
(deftest test-against-corpus ()
  (dolist (test-case *test-corpus*)
    (testing (test-case-description test-case)
      (let ((result (match (test-case-pattern test-case)
                          (test-case-input test-case))))
        (assert-equal (test-case-expected-match test-case)
                     (match-string result))
        (assert-equal (test-case-expected-groups test-case)
                     (match-groups result))))))
```

## Best Practices

1. **Write tests first**: Always start with a failing test
2. **Test boundaries**: Empty strings, single characters, maximum lengths
3. **Test error cases**: Invalid patterns, malformed input
4. **Use properties**: Define mathematical properties your code should satisfy
5. **Benchmark regularly**: Catch performance regressions early
6. **Fuzz continuously**: Run fuzzers in CI to find edge cases
7. **Document test intent**: Clear test names and descriptions
8. **Isolate tests**: No dependencies between tests
9. **Mock external dependencies**: For unit tests
10. **Measure coverage**: But don't optimize for coverage alone