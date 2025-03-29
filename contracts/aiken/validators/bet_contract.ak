use bet_types as t
use aiken/transaction.{OutputReference, ScriptContext, Transaction,Input, Output,InlineDatum,Spend, find_input} as tx
use aiken/math.{max, abs}
use aiken/list
use aiken/interval
use aiken/transaction/value.{lovelace_of, PolicyId, quantity_of}
use aiken/transaction/credential.{from_verification_key, from_script}


const service_percfee: Int = 5
const service_minfee: Int = 2_000_000
const bet_minamount: Int = 5_000_000
const odd_upperbound: Int = 1_000
const odd_bottombound: Int = 100


validator (admin_pkh: t.VerificationKeyHash, treasury_hash: t.VerificationKeyHash, token_policy: PolicyId){
  fn bet_validator(datum: t.BetDatum, redeemer: t.BetRedeemer, ctx: ScriptContext) -> Bool {
    let ScriptContext { transaction, purpose } = ctx
    let Transaction { inputs,extra_signatories,reference_inputs, validity_range .. } = transaction

    expect Spend(script_outref) = purpose
    
    let own_addr = get_ownaddr(inputs,script_outref)
    let fee = max(service_minfee, datum.bet.bet_amount * service_percfee / 100 )
    let trans_fee = lovelace_of(transaction.fee)
    let acceptor_amount = (datum.bet.bet_amount * datum.bet.bet_odds/100) - datum.bet.bet_amount
    let total_amount = acceptor_amount + fee + datum.bet.bet_amount

    expect [Input(_, input_script)] = list.filter(inputs, fn(x) { x.output.address == own_addr })

    list.length(inputs) == 1 &&
    datum.match == redeemer.match &&
    odd_upperbound >= datum.bet.bet_odds &&
    datum.bet.bet_odds >= odd_bottombound &&
    datum.bet.bet_amount >= bet_minamount &&
    datum.match.result_closure > datum.bet.bet_closure &&
  
    when redeemer.action is {
      t.Accept -> {
        let interval.IntervalBound(interval.Finite(validity_upperbound),_) = validity_range.upper_bound
        expect [output_script] = list.filter(transaction.outputs, fn(x) { x.address == own_addr })
        let output_datum = get_datum(output_script)
        datum.bet.bet_status == t.AwaitingAcceptor &&
        datum.bet.bet_closure >= validity_upperbound &&
        lovelace_of(input_script.value) == datum.bet.bet_amount + fee &&
        list.foldr(inputs,0,fn(x, xs) { lovelace_of(x.output.value) + xs }) >= total_amount &&
        lovelace_of(output_script.value) == total_amount &&
        t.BetDatum{..datum, bet: t.BetDetails {..datum.bet, bet_status: t.AwaitingResult}} ==
             t.BetDatum{..output_datum, bet: t.BetDetails {..output_datum.bet, bet_acceptor: datum.bet.bet_acceptor}}    
      }
      t.Cancel -> {

        let creator_input_amount = get_lov_pkh(transaction, "input", datum.bet.bet_creator)
        let creator_output_amount = get_lov_pkh(transaction, "output", datum.bet.bet_creator)

        when datum.bet.bet_status is {
          t.AwaitingAcceptor -> {
            lovelace_of(input_script.value) == datum.bet.bet_amount + fee &&
            list.has(extra_signatories,datum.bet.bet_creator) &&
            creator_input_amount == creator_output_amount + datum.bet.bet_amount + fee - trans_fee
          }
          t.AwaitingResult ->
          {
            let acceptor_input_amount = get_lov_pkh(transaction, "input", datum.bet.bet_acceptor)
            let acceptor_output_amount = get_lov_pkh(transaction, "output", datum.bet.bet_acceptor)
            let creator_refund_diff = creator_input_amount - creator_output_amount + fee + datum.bet.bet_amount
            let acceptor_refund_diff = acceptor_input_amount - acceptor_output_amount + acceptor_amount

            let interval.IntervalBound(interval.Finite(validity_lowerbound),_) = validity_range.lower_bound

            lovelace_of(input_script.value) == datum.bet.bet_amount + fee + acceptor_amount &&
            (list.has(extra_signatories,datum.bet.bet_creator) || list.has(extra_signatories,datum.bet.bet_acceptor)  || list.has(extra_signatories,admin_pkh))&&
            (creator_refund_diff>=0 && acceptor_refund_diff >=0 && creator_refund_diff <= trans_fee && acceptor_refund_diff <= trans_fee) &&
            abs(creator_refund_diff - acceptor_refund_diff) == trans_fee &&
            datum.match.result_closure <= validity_lowerbound

          }
          t.Closed -> False
        }

      }
      t.Close  -> {
          expect [ref_oracle] = list.filter(reference_inputs, fn(x) { x.output.address == from_script(treasury_hash) && quantity_of(x.output.value, token_policy, datum.match.match_id)==1})
          let interval.IntervalBound(interval.Finite(validity_upperbound),_) = validity_range.upper_bound
          let oracle_datum: t.OracleDatum = get_oracledatum(ref_oracle.output)
          let winner_pkh = if (oracle_datum.result == datum.bet.bet_choice) {datum.bet.bet_creator} else {datum.bet.bet_acceptor}
          datum.bet.bet_status == t.AwaitingResult &&
          lovelace_of(input_script.value) == datum.bet.bet_amount + fee + acceptor_amount &&
          oracle_datum.match == datum.match &&
          validity_upperbound <= datum.match.result_closure &&
          get_lov_pkh(transaction, "output_script", treasury_hash) == fee &&
          get_lov_pkh(transaction, "output", winner_pkh) == get_lov_pkh(transaction, "input", winner_pkh) + total_amount - fee - trans_fee &&
          (list.has(extra_signatories,winner_pkh) || list.has(extra_signatories,admin_pkh))
      }
    }
  }
}

fn get_ownaddr(inputs: List<Input>,script_outref: OutputReference){
  expect Some(x) = find_input(inputs,script_outref)
  x.output.address
}

fn get_datum(outp: Output) {
  expect InlineDatum(d) = outp.datum
  expect id: t.BetDatum = d
  id
}

fn get_oracledatum(outp: Output) {
  expect InlineDatum(d) = outp.datum
  expect id: t.OracleDatum = d
  id
}


fn get_lov_pkh(transaction: Transaction, io: ByteArray, pkh: t.VerificationKeyHash){

    if (io =="input"){
      let list_input = list.filter(transaction.inputs, fn(x) { x.output.address == from_verification_key(pkh) })
      list.foldr(list_input,0,fn(x, xs) { lovelace_of(x.output.value) + xs })
      }
    else if (io =="output_script"){
        let list_output = list.filter(transaction.outputs, fn(x) { x.address == from_script(pkh) })
        list.foldr(list_output,0,fn(x, xs) { lovelace_of(x.value) + xs })
    }
    else if (io =="output"){
        let list_output = list.filter(transaction.outputs, fn(x) { x.address == from_verification_key(pkh) })
        list.foldr(list_output,0,fn(x, xs) { lovelace_of(x.value) + xs })
    }
    else {0}
  }