open Std
open Ae_abs_x86_types
module Entity = Ae_entity_std
module Id = Entity.Id
module Ident = Entity.Ident
module Frame = Ae_x86_frame
module Int_table = Entity.Table.Int_table
module Bitvec = Ae_data_bitvec
module Regalloc = Ae_graph_greedy_regalloc
module Graph = Regalloc.Graph
module Mach_reg = Ae_x86_mach_reg
module Id_gen = Entity.Id_gen
module Call_conv = Ae_x86_call_conv
module Chaos_mode = Ae_chaos_mode
module Table = Entity.Ident.Table
module Dominators = Ae_dominators
open Table.Syntax
open Ae_trace

let find_spills func =
  let pred_table = Func.pred_table func in
  let live_in_table, live_out_table = Liveness.compute ~pred_table func in
  let labels = Func.labels_postorder func in
  begin
    let@: label = Vec.iter labels in
    let block = Func.find_block_exn func label in
    ()
  end;
  todo ()
;;
