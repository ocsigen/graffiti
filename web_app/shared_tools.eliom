(* Graffiti
 * http://www.ocsigen.org/graffiti
 * Copyright (C) 2013 Arnaud Parant
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

{shared{

  type messages = (string * float * (float * float) * (float * float))
      deriving (Json)

  let round value = ceil (value -. 0.5)

  (*** images tools ***)

  let max_resolution = 16
  let min_resolution = 9

  (** Calcul min resolution with max value, make cross product **)
  let get_min_resolution max =
    int_of_float (round
                    (( float_of_int (max * min_resolution) ) /.
                        (float_of_int max_resolution) ))

  (** Calcul max resolution with min value, make cross product **)
  let get_max_resolution min =
    int_of_float (round
                    (( float_of_int (min * max_resolution) ) /.
                      (float_of_int min_resolution) ))


}}
