## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(sapevom)

criteria<- c("Cost", "Rating", "Speed")
alternatives<- c("A1", "A2", "A3")

listofmatrices<-list(matrix(c(0,2,1,-2,0,-2,-1,2,0),
                                byrow=TRUE, ncol=3, dimnames=list(criteria)),
                     matrix(c(0,3,-2,-3,0,-3,2,3,0),
                                byrow=TRUE, ncol=3, dimnames=list(criteria))
                     )

listoflistsofmatrices<-list(list(matrix(c(0,2,1,-2,0,-2,-1,2,0),
                                             byrow=TRUE, ncol=3, dimnames=list(alternatives)),
                                 matrix(c(0,3,2,-3,0,-2,-2,2,0),
                                             byrow=TRUE, ncol=3, dimnames=list(alternatives))),
                            list(matrix(c(0,0,1,0,0,2,-1,-2,0),
                                             byrow=TRUE, ncol=3, dimnames=list(alternatives)),
                                 matrix(c(0,-1,2,1,0,3,-2,-3,0),
                                             byrow=TRUE, ncol=3, dimnames=list(alternatives))),
                            list(matrix(c(0,-1,-1,1,0,-1,1,1,0),
                                             byrow=TRUE, ncol=3, dimnames=list(alternatives)),
                                 matrix(c(0,-2,-3,2,0,-3,3,3,0),
                                             byrow=TRUE, ncol=3, dimnames=list(alternatives)))
                            )

sapevom(criteriaEvaluations= listofmatrices, alternativesEvaluations= listoflistsofmatrices)




