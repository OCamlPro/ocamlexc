
(* Path to access stdlib directory *)
let std_lib_path = ref ([] : string list) ;;


let open_in_with_path filename =
 let rec rec_open  = function
  | [] -> raise (Sys_error(filename ^ ": No such file or directory"))
  | h :: rem ->
      try open_in (h ^ filename)
      with Sys_error _ -> rec_open rem in
 if Filename.is_relative filename
 then rec_open !std_lib_path
 else open_in filename
;;
