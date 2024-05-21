module Option = struct
  include Stdlib.Option

  let get ~err = function Some v -> v | None -> failwith err
end
