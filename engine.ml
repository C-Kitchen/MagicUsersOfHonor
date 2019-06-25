type element = Y of el2 | N
and el2 = {name: string; data: el}
and el = Loc of location | Art of artifact | Cre of creature | Spe of spell | Pla of player | Sto of store
and location = {larts: element list; lcres: element list; ldesc1: string; ldesc2: string; llinks: string list; lenter: unit -> unit}
(*Go to checks against llinks, take checks against larts, use checks against inventory then **something in the location, talk to checks against creature list, fight checks against creature list*)
and artifact = {adescL: string; adescI: string; luse: unit -> string}
and creature = {cdesc: string; ctalk: unit -> string; cdeath: unit -> string; catt: string; cmaxhp: int; cnowhp: int; cdamage: int; carmor: int; cprops: creprop list; hostile: bool}(*need speech*)
and creprop = Reflect
and spell = {sdesc: string; sncombat: unit -> string; scombat: spelleffect}
and spelleffect = Damage of int | Flee | Noncombat
and player = {pgold: int; pcombat: creature; pinventory: element list; pspellbook: element list}
and store = {sales: (element * int * bool) list (*true is it stays after bought, false it disappears*); slink: string}
and quest = {qname: string; qcomplete: bool}
;;

let world = ref [];;
let currentloc = ref "";;
let theplayer = ref N;;
let questlog = ref [];;
let gamedone = ref false;;

let trim str =   if str = "" then "" else   let search_pos init p next =
    let rec search i =
      if p i then raise(Failure "empty") else
      match str.[i] with
      | ' ' | '\n' | '\r' | '\t' -> search (next i)
      | _ -> i
    in
    search init   in   let len = String.length str in   try
    let left = search_pos 0 (fun i -> i >= len) (succ)
    and right = search_pos (len - 1) (fun i -> i < 0) (pred)
    in
    String.sub str left (right - left + 1)   with   | Failure "empty" -> "" ;;

let rec wordwrap ts =
	let s = trim ts in
	if String.length s < 80
		then [s]
		else
			let lastspace = String.rindex (String.sub s 0 80) ' ' in
			(String.sub s 0 lastspace)::(wordwrap (String.sub s lastspace ((String.length s) - lastspace)));;
	
let print_snl s =
	let pr ts = print_string ts; print_newline () in
	List.iter pr (wordwrap s);;

let contain bigs lits =
	let litlen = String.length lits in
	let biglen = String.length bigs in
	let biguncap = String.lowercase bigs in
	let lituncap = String.lowercase lits in
	if litlen > biglen
		then false
		else
			let rec iter i =
				if i > biglen - litlen
					then false
					else
						if String.sub biguncap i litlen = lituncap
							then true
							else iter (i+1) in
			iter 0;;
				
let getinput () = print_string ": "; read_line ();;

let rec findelement text ellist =
	match ellist with
			[] -> (N, [])
		| h::t -> match h with
					N -> findelement text t
				| Y el ->
						if contain text el.name
							then (h, t)
							else (fun (e, l) -> (e, h::l)) (findelement text t);;
							
(*let loclist = List.map (fun e -> e.contents);;*)

let addlink2 name1 name2 =
	let (link1, rest1) = findelement name1 world.contents in
	let (link2, rest2) = findelement name2 rest1 in
	let alink a l =
		if List.mem a l then l else a::l in
	let makelink element1 nameofelement2 =
		match element1 with
				N -> element1
			| Y el ->
					match el.data with
							Loc l -> Y {name = el.name; data = Loc {larts = l.larts; lcres = l.lcres; ldesc1 = l.ldesc1; ldesc2 = l.ldesc2; llinks = (alink (nameofelement2) l.llinks); lenter = l.lenter}}
						| _ -> element1 in	
	world := (makelink link1 name2)::(makelink link2 name1)::rest2;;

let getloc name = (fun (x,_) -> x) (findelement (name.contents) (world.contents));;

let here () = (fun x -> match x with Y y -> (match y.data with Loc l -> l | _ -> failwith "line 106.1") | _ -> failwith "line 106.2") (getloc currentloc);;

let deleteinv str = match theplayer.contents with
		N -> ()
	| Y el -> (match el.data with
				Pla p -> theplayer:= (Y {name = el.name; data = Pla {pgold = p.pgold; pcombat = p.pcombat; pinventory = ((fun (_,x) -> x) (findelement str p.pinventory)) ; pspellbook = p.pspellbook}})
			|	_ -> ());;

let addtoinv item = match theplayer.contents with
		N -> ()
	| (Y el)  ->
			match el.data with
					Pla play -> theplayer := (Y {name = el.name; data = Pla {pgold = play.pgold; pcombat = play.pcombat; pinventory = item::play.pinventory; pspellbook = play.pspellbook}})
				| _ -> ();;

let addspell spe = match theplayer.contents with
		N -> ()
	| (Y el)  ->
			match el.data with
					Pla play -> theplayer := (Y {name = el.name; data = Pla {pgold = play.pgold; pcombat = play.pcombat; pinventory = play.pinventory; pspellbook = spe::play.pspellbook}})
				| _ -> ();;

let changetalk place person talk =
	match findelement place (world.contents) with (foundl, restl) ->
	match foundl with
			N -> ()
		| Y x -> (match x.data with
				Loc l ->
					let (foundp, restp) = findelement person (l.lcres) in
					(match foundp with
							N -> ()
						| Y y -> (match y.data with
									Cre c ->
										let newperson = Y {name = y.name; data = Cre {cdesc = c.cdesc; ctalk = talk; cdeath = c.cdeath; catt = c.catt; cmaxhp = c.cmaxhp; cnowhp = c.cnowhp; cdamage = c.cdamage; carmor = c.carmor; cprops = c.cprops; hostile = c.hostile}} in
										world := (Y {name = x.name; data = Loc {larts = l.larts; lcres = newperson::restp; ldesc1 = l.ldesc1; ldesc2 = l.ldesc2; llinks = l.llinks; lenter = l.lenter}})::restl
								| _ -> ()))
			|	_ -> ());;

let changecdesc place person desc =
	match findelement place (world.contents) with (foundl, restl) ->
	match foundl with
			N -> ()
		| Y x -> (match x.data with
				Loc l ->
					let (foundp, restp) = findelement person (l.lcres) in
					(match foundp with
							N -> ()
						| Y y -> (match y.data with
									Cre c ->
										let newperson = Y {name = y.name; data = Cre {cdesc = desc; ctalk = c.ctalk; cdeath = c.cdeath; catt = c.catt; cmaxhp = c.cmaxhp; cnowhp = c.cnowhp; cdamage = c.cdamage; carmor = c.carmor; cprops = c.cprops; hostile = c.hostile}} in
										world := (Y {name = x.name; data = Loc {larts = l.larts; lcres = newperson::restp; ldesc1 = l.ldesc1; ldesc2 = l.ldesc2; llinks = l.llinks; lenter = l.lenter}})::restl
								| _ -> ()))
			|	_ -> ());;


let deletestore st a =
	match findelement st (world.contents) with (founds, restl) ->
	match founds with
			N -> ()
		| Y x -> (match x.data with
				Sto s ->
					let resti = List.filter (fun (x,_,_) -> match x with Y y -> not(y.name = a) | _ -> failwith "line 167.1") s.sales in
					world := (Y {name = x.name; data = Sto {sales = resti; slink = s.slink}})::restl
			|	_ -> ());;

let addperson place person =
	match findelement place (world.contents) with (foundl, restl) ->
	match foundl with
			N -> ()
		| Y x -> (match x.data with
				Loc l ->
					world := (Y {name = x.name; data = Loc {larts = l.larts; lcres = person::l.lcres; ldesc1 = l.ldesc1; ldesc2 = l.ldesc2; llinks = l.llinks; lenter = l.lenter}})::restl
			|	_ -> ());;

let addartifact place artif =
	match findelement place (world.contents) with (foundl, restl) ->
	match foundl with
			N -> ()
		| Y x -> (match x.data with
				Loc l ->
					world := (Y {name = x.name; data = Loc {larts = artif::l.larts; lcres = l.lcres; ldesc1 = l.ldesc1; ldesc2 = l.ldesc2; llinks = l.llinks; lenter = l.lenter}})::restl
			|	_ -> ());;
												
let checkquest n =
	List.exists (fun x -> (x.qname = n) && (x.qcomplete)) questlog.contents;;
	
let completequest n =
	let rec findq l =
		match l with
				[] -> failwith "no such quest"
			| h::t -> if h.qname = n then (h.qname, t) else (fun (x,y) -> (x, h::y)) (findq t) in
	try
		(match findq questlog.contents with (q, rest) -> questlog := {qname = q; qcomplete = true}::rest)
	with
		_ -> ();;
			
let haselement name l =
	if ((fun (x,_) -> x) (findelement name l)) = N then false else true;;

let checkinv name = match theplayer.contents with
		N -> false
	| Y el -> (match el.data with
				Pla p -> haselement name p.pinventory
			|	_ -> false);;
	
let goldchange g = match theplayer.contents with
		N -> ()
	| Y el -> (match el.data with
				Pla p -> theplayer:= (Y {name = el.name; data = Pla {pgold = p.pgold + g; pcombat = p.pcombat; pinventory = p.pinventory; pspellbook = p.pspellbook}})
			|	_ -> ());;

let hpchange d = match theplayer.contents with
		N -> false
	| Y el -> match el.data with
				Pla p ->
					if (p.pcombat).cnowhp < (p.pcombat).cmaxhp
					then
						(theplayer := (Y {name = el.name; data = Pla {pgold = p.pgold; pcombat = {cdesc = (p.pcombat).cdesc; ctalk = (p.pcombat).ctalk; cdeath = (p.pcombat).cdeath; catt = (p.pcombat).catt; cmaxhp = (p.pcombat).cmaxhp; cnowhp = min ((p.pcombat).cnowhp + d) (p.pcombat).cmaxhp; cdamage = (p.pcombat).cdamage; carmor = (p.pcombat).carmor; cprops = (p.pcombat).cprops; hostile = (p.pcombat).hostile}; pinventory = p.pinventory; pspellbook = p.pspellbook}});
						true)
					else false
					
			|	_ -> false;;
			
let hpset hp = match theplayer.contents with
		N -> ()
	| Y el -> (match el.data with
				Pla p -> theplayer:= (Y {name = el.name; data = Pla {pgold = p.pgold; pcombat = {cdesc = (p.pcombat).cdesc; ctalk = (p.pcombat).ctalk; cdeath = (p.pcombat).cdeath; catt = (p.pcombat).catt; cmaxhp = (p.pcombat).cmaxhp; cnowhp = min hp (p.pcombat).cmaxhp; cdamage = (p.pcombat).cdamage; carmor = (p.pcombat).carmor; cprops = (p.pcombat).cprops; hostile = (p.pcombat).hostile}; pinventory = p.pinventory; pspellbook = p.pspellbook}})
			|	_ -> ());;

let delcre cname lname =
	let (l1, restl) = findelement lname world.contents in
		match l1 with
				N -> ()
			| Y el -> match el.data with
						Loc l ->
							let (_, restc) = findelement cname l.lcres in
							let newl = Y {name = el.name; data = Loc {larts = l.larts; lcres = restc; ldesc1 = l.ldesc1; ldesc2 = l.ldesc2; llinks = l.llinks; lenter = l.lenter}} in
							world := (newl)::restl
					| _ -> ();;

let delart aname lname =
	let (l1, restl) = findelement lname world.contents in
		match l1 with
				N -> ()
			| Y el -> match el.data with
						Loc l ->
							let (_, resta) = findelement aname l.larts in
							let newl = Y {name = el.name; data = Loc {larts = resta; lcres = l.lcres; ldesc1 = l.ldesc1; ldesc2 = l.ldesc2; llinks = l.llinks; lenter = l.lenter}} in
							world := (newl)::restl
					| _ -> ();;
					
let addplayprop pr = match theplayer.contents with
		N -> ()
	| Y el -> (match el.data with
				Pla p ->
					let newprops = if List.mem pr (p.pcombat).cprops then (p.pcombat).cprops else pr::(p.pcombat).cprops in
					theplayer:= (Y {name = el.name; data = Pla {pgold = p.pgold; pcombat = {cdesc = (p.pcombat).cdesc; ctalk = (p.pcombat).ctalk; cdeath = (p.pcombat).cdeath; catt = (p.pcombat).catt; cmaxhp = (p.pcombat).cmaxhp; cnowhp = (p.pcombat).cnowhp; cdamage = (p.pcombat).cdamage; carmor = (p.pcombat).carmor; cprops = newprops; hostile = (p.pcombat).hostile}; pinventory = p.pinventory; pspellbook = p.pspellbook}})
			|	_ -> ());;

let addplayweapon newcatt damage = match theplayer.contents with
		N -> ()
	| Y el -> (match el.data with
				Pla p ->
					theplayer:= (Y {name = el.name; data = Pla {pgold = p.pgold; pcombat = {cdesc = (p.pcombat).cdesc; ctalk = (p.pcombat).ctalk; cdeath = (p.pcombat).cdeath; catt = newcatt; cmaxhp = (p.pcombat).cmaxhp; cnowhp = (p.pcombat).cnowhp; cdamage = damage; carmor = (p.pcombat).carmor; cprops = (p.pcombat).cprops; hostile = (p.pcombat).hostile}; pinventory = p.pinventory; pspellbook = p.pspellbook}})
			|	_ -> ());;
			
let gethp () = match theplayer.contents with
		N -> 0
	| Y el -> (match el.data with
				Pla p -> (p.pcombat).cnowhp
			|	_ -> 0);;

let getmaxhp () = match theplayer.contents with
		N -> 0
	| Y el -> (match el.data with
				Pla p -> (p.pcombat).cmaxhp
			|	_ -> 0);;
			
let changeloc loc = currentloc := loc;;
			
let reset () = print_newline (); print_string "----------"; print_newline (); print_newline ();;

let game startingworld start gamename quests startingplayer =  (*startingworld is a ((element ref) list), and only contains locations, start is the name of the starting location*)
	world := startingworld;
	currentloc := start;
	questlog := List.map (fun x -> {qname = x; qcomplete = false}) quests;
	let makename () =
		reset ();
		print_snl (String.concat " " ["Welcome to"; gamename]);
		print_snl "Input character name";
		getinput () in
	theplayer := (Y {name = makename (); data = startingplayer});
	let showquests () = List.iter (fun a -> print_snl (String.concat " " [a.qname; if a.qcomplete then "complete" else "NOT complete"])) questlog.contents in
	let gamecomplete () = List.for_all (fun a -> a.qcomplete) questlog.contents && (not !gamedone) in
	let combat pla planame cre crename = (*need to fix this*)
		let phpinit = pla.cnowhp in
		let chpinit = cre.cnowhp in
		let comlist = ("Attack",pla.catt, Damage (pla.cdamage))::(List.filter (fun (_,_,x) -> not(x = Noncombat)) (List.map (fun x -> match x with Y y -> (match y.data with Spe s -> String.concat " " ["Cast";y.name], s.sdesc, s.scombat | _ -> failwith "line 304.1") | _ -> failwith "line 304.2") (((fun x -> match x with Y y -> (match y.data with Pla p -> p.pspellbook | _ -> failwith "line 304.3") | _ -> failwith "line 304.4")) theplayer.contents))) in
		let rec dispcom i l =
			match l with
					[] -> ()
				| (name,desc,_)::t ->
						print_snl (String.concat " " [string_of_int i; ":"; name; "-"; desc]);
						dispcom (i+1) t in			
		let rec turn php chp disp credisp =
			reset ();
			print_snl disp;
			print_snl (String.capitalize credisp);
			print_newline ();
			print_snl (String.concat " " [planame;"HP:";string_of_int php]);
			print_snl (String.concat " " [(String.capitalize crename);"HP:";string_of_int chp]);
			print_newline ();
			if php < 1
				then (print_snl "GAME OVER"; "Thank you for playing.")
				else if chp < 1
					then
						(delcre crename currentloc.contents;
						hpset php;
						cre.cdeath ())
					else
						(dispcom 0 comlist;
						let typed = getinput () in
						try
							let opt = int_of_string typed in
							if (opt < (List.length comlist)) && (opt >= 0)
								then
									let (command, _, effect) = List.nth comlist opt in
										match effect with
												Damage pdam ->
													if (List.mem Reflect cre.cprops) && ((String.sub command 0 4) = "Cast")
														then
															let newphp = php - (max 0 (pdam - pla.carmor)) - (max 0 (cre.cdamage - pla.carmor)) in
															turn newphp chp (String.concat "" [command;", but it reflects off the shield."]) (String.concat " " [crename;cre.catt])
														else
															if (List.mem Reflect pla.cprops) && ((String.sub cre.catt 0 4) = "cast")
																then
																	let newchp = chp - (max 0 (pdam - cre.carmor)) - (max 0 (cre.cdamage - cre.carmor)) in
																	turn php newchp command (String.concat "" [crename;" ";cre.catt;", but it reflects off the shield."])
																else
																	let newchp = chp - (max 0 (pdam - cre.carmor)) in
																	let newphp = php - (max 0 (cre.cdamage - pla.carmor)) in
																	turn newphp newchp command (String.concat " " [crename;cre.catt])
											| Flee ->
													(*change creature's hp*)
													hpset php;
													currentloc := "plaza"; (*this line shouldn't be in a general engine, it's just a terrible work around*)
													"You get away."
											| _ ->
													turn php chp "Invalid command." ""
								else turn php chp "Invalid command." ""
						with _ -> turn php chp "Invalid command." "") in
		turn phpinit chpinit "" "" in
	let rec encounter creatures =
		match creatures with
				[] -> N
			| h::t -> (match h with
						N -> encounter t
					| Y el -> (match el.data with
							Cre c -> if c.hostile then h else encounter t
						| _ -> encounter t)) in
	let makeitemlist items place =
		let (l1, rest) = findelement (place.contents) (world.contents) in
		let newl1 = match l1 with
				N -> l1
			| Y el -> match el.data with
					Loc loc1 -> Y {name = el.name; data = Loc {larts = items; lcres = loc1.lcres; ldesc1 = loc1.ldesc1; ldesc2 = loc1.ldesc2; llinks = loc1.llinks; lenter = loc1.lenter}}
				| _ -> l1 in
		world := (newl1::rest) in
	let inventory () =
		print_snl (String.concat " " ["HP:";string_of_int (gethp ());"/";string_of_int (getmaxhp ())]);
		print_snl (String.concat " " ["Gold:";string_of_int ((fun x -> match x with Y y -> (match y.data with Pla y -> y.pgold | _ -> failwith "line 377.1") | _ -> failwith "line 377.2") (theplayer.contents))]);
		List.iter (fun x -> match x with Y y -> (match y.data with Art a -> print_snl (String.capitalize (String.concat "" [y.name; ": "; a.adescI])) | _ -> failwith "line 378.1") | _ -> failwith "line 378.2") (match !theplayer with Y y -> (match y.data with Pla p -> p.pinventory | _ -> failwith "line 378.3") | _ -> failwith "line 378.4") in
	let spellbook () =
		List.iter (fun x -> match x with Y y -> (match y.data with Spe s -> print_snl (String.capitalize (String.concat "" [y.name; ": "; s.sdesc])) | _ -> failwith "line 380.1") | _ -> failwith "line 380.2") (match !theplayer with Y y -> (match y.data with Pla y -> y.pspellbook | _ -> failwith "line 378.3") | _ -> failwith "line 380.4") in
	let shop s =
		let rec dispshop i l =
			match l with
					[] -> print_snl (String.concat " " [string_of_int i; ":"; "Exit to"; s.slink])
				| h::t -> (match h with
							(N, _, _) -> dispshop (i+1) t
						| (Y el, price, _) ->
								let description = (match el.data with
										Art a -> a.adescI
									| Spe s -> s.sdesc
									| _ -> "Invalid Item") in
								print_snl (String.concat " " [string_of_int i; ":"; string_of_int price; "gp, "; String.capitalize el.name; "-"; description]);
								dispshop (i+1) t) in
		print_snl (String.capitalize currentloc.contents);
		print_snl (String.concat " " ["Your gold:";string_of_int ((fun x -> match x with Y y -> (match y.data with Pla p -> p.pgold | _ -> failwith "line 395.1") | _ -> failwith "line 395.2") theplayer.contents)]);
		dispshop 0 s.sales;
		let typed = getinput () in
		try
			let opt = int_of_string typed in
			if (opt < (List.length s.sales)) && (opt >= 0)
				then
					let (buy, price, resale) = List.nth s.sales opt in
					if price > ((fun x -> match x with Y y -> (match y.data with Pla p -> p.pgold | _ -> failwith "line 403.1") | _ -> failwith "line 403.2") theplayer.contents)
						then ("Not enough gold", typed)
						else
							match buy with
									N -> ("Invalid item in store", typed)
								| Y el ->
										goldchange (-price);
										if resale then () else deletestore currentloc.contents el.name; (*delete item from store*)
										let _ = match el.data with
												Spe _ -> addspell buy
											| Art _ -> addtoinv buy
											| _ -> goldchange price in
										(String.concat "" [el.name;" bought"], typed)
				else if opt = (List.length s.sales)
					then (currentloc := s.slink; ("Thank you, come again", typed))
					else ("Invalid option", typed)
		with _ -> ("Invalid option", typed) in
		(*(display,typed)*)
		
	let commands () =
		print_snl "Help: displays this list of commands";
		print_snl "Take ##: adds item to your inventory";
		print_snl "Go to ## or goto ##: moves to location";
		print_snl "Use ##: uses item from your inventory (note you must use new weapons and such in order to equip them)";
		print_snl "Inventory: displays items in your inventory";
		print_snl "Cast ##: casts the spell from your spellbook";
		print_snl "Spellbook: displays the spells you know";
		print_snl "Talk to ##: talk to creature";
		print_snl "Fight ##: fights creature";
		print_snl "Quit: quit the game" in
	let finishgame () = 
		reset ();
		gamedone := true;
		print_snl "You have completed all quests in the game. Note that some quests can be completed in multiple ways, so feel free to play again. Quit now? (y/n)";
		let rec x () = let k = try (String.lowercase (getinput ())).[0] with _ -> ' ' in
			if k = 'y'
				then "Thank you for playing."
				else if k = 'n'
					then ""
					else x () in
		x () in
	let quit () =
		reset ();
		print_snl "Are you sure you want to quit? (y/n)";
		let rec x () = let k = try (String.lowercase (getinput ())).[0] with _ -> ' ' in
			if k = 'y'
				then "Thank you for playing."
				else if k = 'n'
					then ""
					else x () in
		x () in
	let execute command =
		let lc = (fun x -> match x with Y y -> (match y.data with Loc l -> l | _ -> failwith "line 454.1") | _ -> failwith "line 454.2") (getloc currentloc) in
		let comlen = String.length command in
		let keyword = if comlen < 3 then "" else String.lowercase (String.sub command 0 3) in
		let words = if comlen < 4 then "" else String.sub command 3 (comlen - 3) in
		match keyword with
				"tak" -> let (found, rest) = findelement words lc.larts in
					(match found with
							N -> "You cannot take that."
						| Y el ->
								addtoinv found;
								 makeitemlist rest currentloc;
									String.concat " " ["You take the"; el.name])
			| "use" -> let (found, rest) = findelement words (match !theplayer with Y y -> (match y.data with Pla p -> p | _ -> failwith "line 466.1") | _ -> failwith "line 466.2").pinventory in
					(match found with
							N -> "That item is not in your inventory."
						| Y el ->
								(match el.data with
										Art thing -> thing.luse ()
									| _ -> "Invalid object in inventory."))
			| "tal" -> let (found, rest) = findelement words lc.lcres in
					(match found with
							N -> "That cannot talk to that."
						| Y el ->
								(match el.data with
										Cre thing -> thing.ctalk ()
									| _ -> "Invalid creature in location."))
			| "got" -> let (found, _) = try findelement (List.find (fun x -> contain words x) lc.llinks) (world.contents) with _ -> (N,[]) in
					(match found with
							N -> "Cannot get to there from here."
						| Y el ->
								currentloc := el.name;
								"")
			| "go " -> let (found, _) = try findelement (List.find (fun x -> contain words x) lc.llinks) (world.contents) with _ -> (N,[]) in
					(match found with
							N -> "Cannot get to there from here."
						| Y el ->
								currentloc := el.name;
								"")
			| "fig" -> let (found, rest) = findelement words lc.lcres in
					(match found with
							N -> "You cannot fight that."
						| Y el -> combat (match !theplayer with Y y -> (match y.data with Pla p -> p.pcombat | _ -> failwith "line 495.1") | _ -> failwith "line 495.2") (match !theplayer with Y y -> y.name | _ -> failwith "line 495.3") (match el.data with Cre c -> c | _ -> failwith "line 495.4") el.name)
			| "cas" -> let (found, rest) = findelement words (match !theplayer with Y y -> (match y.data with Pla p -> p | _ -> failwith "line 496.1") | _ -> failwith "line 496.2").pspellbook in
					(match found with
							N -> "You do not know that spell."
						| Y el ->
								(match el.data with
										Spe thing -> thing.sncombat ()
									| _ -> "Invalid spell in spellbook."))
			| "qui" -> quit ()
			| "inv" -> "Inventory:"
			| "spe" -> "Spellbook:"
			| "hel" -> "Command List:"
			| "que" -> "Quests:"
			| _ -> "Invalid command." in
	let rec go disp prev =
		reset ();
		print_snl disp;
		if disp = "Inventory:" then inventory () else ();
		if disp = "Spellbook:" then spellbook () else ();
		if disp = "Command List:" then commands () else ();
		if disp = "Quests:" then showquests () else ();
		print_snl "";
		let lc =
			match getloc currentloc with
					N -> failwith "Invalid location" 
				| Y el -> el.data in
						(match lc with
								Loc lo ->
									lo.lenter ();
									
									print_snl (String.concat " " ([lo.ldesc1]@(List.map (fun x -> match x with Y y -> (match y.data with Art a -> a.adescL | _ -> failwith "line 525.1") | _ -> failwith "line 525.2") (lo.larts))
										@(List.map (fun x -> match x with Y y -> (match y.data with Cre c -> c.cdesc | _ -> failwith "line 526.1") | _ -> failwith "line 526.2") (lo.lcres))@[lo.ldesc2]));
									print_snl "";
									print_snl "Nearby locations:";
									List.iter (fun x ->  print_snl (String.capitalize x)) (lo.llinks);
									print_snl "";
									let typed = getinput () in
									let display = 
										match encounter lo.lcres with
												N -> if gamecomplete () then finishgame () else execute typed
											| Y el -> combat (match !theplayer with Y y -> (match y.data with Pla p -> p.pcombat | _ -> failwith "line 535.1") | _ -> failwith "line 535.2") (match !theplayer with Y y -> y.name | _ -> failwith "line 535.3") (match el.data with Cre c -> c | _ -> failwith "line 535.4") el.name
										 in
									if display = "Thank you for playing."
										then (print_snl display; exit 0)
										else go display typed
							| Sto st -> let (display, typed) = shop st in go display typed
							| _ -> failwith "Invalid Location" ) in
	go "Command List:" "";;