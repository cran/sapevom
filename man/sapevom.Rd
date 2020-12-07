\name{sapevom}
\alias{sapevom}

\title{
SAPEVO-M
}
\description{
Implements SAPEVO-M method
}
\usage{
sapevom(criteriaEvaluations, alternativesEvaluations)
}

\arguments{
  \item{criteriaEvaluations}{
a list of matrices with pairwise comparison of criteria, one matrix for each decisor.
}
  \item{alternativesEvaluations}{
a list of lists of matrices with pairwise comparison of alternatives, one list for each criterion containing one matrix for each decisor.
}
}
\details{
Criteria and alternatives must be in the same order on all the matrices. Define row names for all matrices, like on documentation example, to have a more understandable output. The pairwise comparisons must be made with a scale of seven values, from -3 to 3, to represent the preferences.
}
\value{
a vector with citeria weights and a dataframe with SAPEVO-M results.
}
\references{
GOMES, C. F. S., DOS SANTOS, M., TEIXEIRA, L. F. H. S. B.,  SANSEVERINO, A. M. and BARCELOS, M.R. S. (2020). SAPEVO-M: a group multicriteria ordinal ranking method. Pesquisa Operacional. 40. 1-20. DOI: 10.1590/0101-7438.2020.040.00226524.

}
\author{Raquel Coutinho \email{rdouradocoutinho@gmail.com},
        Marcos do Santos \email{marcosdossantos@ime.eb.br}}


\examples{

criteria<- c("C1", "C2", "C3")
alternatives<- c("A1", "A2", "A3")

listofmatrices<-list(matrix(c(0,1,1,-1,0,0,-1,0,0),
                                byrow=TRUE, ncol=3, dimnames=list(criteria)),
                     matrix(c(0,2,1,-2,0,1,-1,-1,0),
                                byrow=TRUE, ncol=3, dimnames=list(criteria))
                     )

listoflistsofmatrices<-list(list(matrix(c(0,0,-1,0,0,2,1,-2,0),
                                             byrow=TRUE, ncol=3, dimnames=list(alternatives)),
                                 matrix(c(0,-1,2,1,0,0,-2,0,0),
                                             byrow=TRUE, ncol=3, dimnames=list(alternatives))),
                            list(matrix(c(0,1,0,-1,0,0,0,0,0),
                                             byrow=TRUE, ncol=3, dimnames=list(alternatives)),
                                 matrix(c(0,2,1,-2,0,1,-1,-1,0),
                                             byrow=TRUE, ncol=3, dimnames=list(alternatives))),
                            list(matrix(c(0,3,2,-3,0,1,-2,-1,0),
                                             byrow=TRUE, ncol=3, dimnames=list(alternatives)),
                                 matrix(c(0,0,3,0,0,-1,-3,1,0),
                                             byrow=TRUE, ncol=3, dimnames=list(alternatives)))
                            )

sapevom(criteriaEvaluations= listofmatrices, alternativesEvaluations= listoflistsofmatrices)


}

