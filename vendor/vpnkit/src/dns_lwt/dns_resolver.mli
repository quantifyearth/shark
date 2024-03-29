(*
 * Copyright (c) 2012 Richard Mortier <mort@cantab.net>
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014-2015 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

type commfn = {
  txfn    : Cstruct.t -> unit Lwt.t;
  (** [txfn buf] resolves when [buf] has been transmitted. *)

  rxfn    : (Cstruct.t -> Vpnkit_dns.Packet.t option) -> Vpnkit_dns.Packet.t Lwt.t;
  (** [rxfn parse] resolves to a packet processed by [parse] after it
      has been received. *)

  timerfn : unit -> unit Lwt.t;
  (** [timerfn ()] resolves when a request should be timed out. *)

  cleanfn : unit -> unit Lwt.t;
  (** [cleanfn ()] resolves after any resources used by the rest of
      the {!commfn} have been released. *)
}
(** A [commfn] value describes the means by which datagram
    transmission and reception occur as well as providing functions to
    timeout requests and clean up any resources used. *)

val resolve_pkt :
  (module Vpnkit_dns.Protocol.CLIENT) ->
  ?alloc:(unit -> Cstruct.t) ->
  commfn -> Vpnkit_dns.Packet.t ->
  Vpnkit_dns.Packet.t Lwt.t
(** [resolve_pkt client ?alloc commfn packet] will attempt resolution
    of the query contained in [packet] via the protocol client
    [client] and using the utilities of [commfn]. [alloc] may be
    provided to control how buffers are allocated.

    [client] may issue multiple network transactions for the same
    query simultaneously. The first received successful response will
    be returned. If no responses are successful or received before
    {!commfn.timerfn} resolves, the {!Lwt.t} value will fail with a
    {!Vpnkit_dns.Protocol.Dns_resolve_error} exception which contains a list
    of all of the errors encountered during resolution. *)

val resolve : 
  (module Vpnkit_dns.Protocol.CLIENT) ->
  ?alloc:(unit -> Cstruct.t) ->
  ?dnssec:bool ->
  commfn -> Vpnkit_dns.Packet.q_class -> 
  Vpnkit_dns.Packet.q_type -> 
  Vpnkit_dns.Name.t ->
  Vpnkit_dns.Packet.t Lwt.t
(** [resolve client ?alloc ?dnssec commfn q_class q_type name] will
    construct a query packet from [dnssec], [q_class], [q_type], and
    [name] and then attempt to resolve it by calling {!resolve_pkt}. *)

val gethostbyname :
  ?alloc:(unit -> Cstruct.t) ->
  ?q_class:Vpnkit_dns.Packet.q_class ->
  ?q_type:Vpnkit_dns.Packet.q_type -> commfn ->
  string -> Ipaddr.t list Lwt.t

val gethostbyaddr :
  ?alloc:(unit -> Cstruct.t) ->
  ?q_class:Vpnkit_dns.Packet.q_class ->
  ?q_type:Vpnkit_dns.Packet.q_type -> commfn ->
  Ipaddr.V4.t -> string list Lwt.t
