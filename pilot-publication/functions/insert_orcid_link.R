insert_orcid_link <- function(orcid.id) {
    return(paste0("[![](https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png){height=16 width=16}](https://orcid.org/", orcid.id, ")"))
    # return(paste0("[![](ORCIDiD_icon16x16.png){height=16 width=16}](https://orcid.org/", orcid.id, ")"))
}
