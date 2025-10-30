#' bday_sim
#' for a fixed group size, this function randomly draws “birthdays” for each person and checks whether there are duplicate birthdays in the group
#' @param n_room parameter for group size in room
#'
#' @returns TRUE or FALSE based on presense of duplicate birthdays in sample
#' @export repeated_bday
#'
#' @examples
bday_sim = function(n_room){
  
  birthdays = sample(1:365, n_room, replace = TRUE)
  
  repeated_bday = length(unique(birthdays)) < n_room
  
  repeated_bday
}