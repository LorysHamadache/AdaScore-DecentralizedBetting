use bet_types.{VerificationKeyHash}
use aiken/transaction.{ScriptContext,Datum, Redeemer, Transaction} as tx
use aiken/list

validator (admin_pkh: VerificationKeyHash){
  fn treasury_validator(_: Datum, _r: Redeemer, ctx: ScriptContext) -> Bool {
    let ScriptContext { transaction, .. } = ctx
    let Transaction { extra_signatories, .. } = transaction
    list.has(extra_signatories, admin_pkh)
  }
}