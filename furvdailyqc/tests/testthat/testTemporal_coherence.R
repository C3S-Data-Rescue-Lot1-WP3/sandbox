context('temporal_coherence')

function (temporal_coherence){
  if(not_working()) {
    skip('API not available')
  }
}
test_correct <- function(check_code, diagnose_code) {
  check_res <- run_until_fail(substitute(check_code))
  diagnose_res <- run_until_fail(substitute(diagnose_code))
  if (check_res$correct) {
    # all good
  } else {
    if (diagnose_res$correct) {
      check_that(failure(), feedback = check_res$feedback)
    } else {
      check_that(failure(), feedback = diagnose_res$feedback)
    }
  }
}

test_or <- function(temporal_coherence, incorrect_msg = NULL, choose_feedback = 1) {

  input <- substitute(alist(...))
  input[[1]] <- NULL

  passes <- logical(length(input) )
  feedback <- list()

  for (i in seq_along(input)) {
    code <- input[[i]]
    res <- run_until_fail(code)
    passes[i] <- res$correct
    feedback[[i]] <- res$feedback
  }

  if (!any(passes)) {
    if (is.null(incorrect_msg)) {
      check_that(failure(), feedback = feedback[[choose_feedback]])
    } else {
      check_that(failure(), feedback = incorrect_msg)
    }
  }
}
