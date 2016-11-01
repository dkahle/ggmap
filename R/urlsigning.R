#' Sign a url using a google digital signature
#' @author Richard Beare
#' @param input_url The url to be signed. Should include a client field.
#' @param secret The private key
#' @return The signed url
#' @details Derived from the python urlsigner: 
#' https://raw.githubusercontent.com/googlemaps/js-v2-samples/61b3f58eb1286a428843f8401048226b8648a76b/urlsigning/urlsigner.py
#' 
signurl <- function(input_url, secret)
{
  secret.safe <- chartr("-_", "+/", secret)
  decoded_key <- base64enc::base64decode(secret.safe)
  # break up the url
  urlparsed <- urltools::url_parse(input_url)
  url_to_sign <- paste0("/", urlparsed$path, "?", urlparsed$parameter)
  signature <- digest::hmac(decoded_key, url_to_sign, algo="sha1", raw=TRUE)
  enc1 <- base64enc::base64encode(signature, linewidth=NA)
  urlsafesig <- chartr("+/", "-_",  enc1)
  originalurl <- urltools::url_compose(urlparsed)
  finalurl <- paste0(originalurl, "&signature=", urlsafesig)
  return(finalurl)
}
