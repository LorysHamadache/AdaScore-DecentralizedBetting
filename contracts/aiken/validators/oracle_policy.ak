use bet_types as t
use aiken/transaction.{ScriptContext, Transaction, Datum,InlineDatum, Mint} as tx
use aiken/list
use aiken/dict
use aiken/transaction/value.{quantity_of}
use aiken/transaction/credential.{from_script}


//Done
validator(admin_pkh: t.VerificationKeyHash, treasury_hash: t.VerificationKeyHash) {

  fn mint_oracle(rdmr: t.OracleRedeemer, ctx: ScriptContext) -> Bool {
    
    let ScriptContext { transaction, purpose } = ctx
    let Transaction { mint, extra_signatories, .. } = transaction    

    expect Mint(policy_id) = purpose
    expect [(asset_name, amount)] = // Technically check that there is only one asset name minted
      mint
        |> value.from_minted_value
        |> value.tokens(policy_id)
        |> dict.to_list()
    
    list.has(extra_signatories, admin_pkh) &&
    when rdmr is {
      t.MintOracle ->
      { 
        expect [output_wtoken] = list.filter(transaction.outputs, fn(x) {quantity_of(x.value, policy_id, asset_name)==1})
        let oracle_datum: t.OracleDatum = get_datum(output_wtoken.datum)
        amount == 1 && 
        asset_name == oracle_datum.match.match_id &&
        output_wtoken.address == from_script(treasury_hash)
      }
      t.BurnOracle ->
        amount == -1
    }
  }
}

fn get_datum(dat: Datum)
{
  expect InlineDatum(d) = dat
  expect id: t.OracleDatum = d
  id
}

