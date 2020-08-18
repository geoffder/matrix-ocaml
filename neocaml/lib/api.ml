open Base

let matrix_api_path   = "/_matrix/client/r0"
let matrix_media_path = "/_matrix/media/r0"

let build_path ?query_parameters ?api_path:(api=matrix_api_path) path =
  query_parameters
  |> Option.map ~f:Uri.encoded_of_query
  |> Option.value ~default:""
  |> ( ^ ) @@ api ^ "/" ^ Uri.pct_encode path ^ "?"
