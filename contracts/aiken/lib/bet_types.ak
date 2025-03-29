use aiken/transaction/credential.{VerificationKey}
use aiken/hash.{Blake2b_224, Hash}
use aiken/time.{PosixTime}

pub type VerificationKeyHash =
  Hash<Blake2b_224, VerificationKey>

pub type BetStatus {
  AwaitingAcceptor
  AwaitingResult
  Closed
}

pub type MatchDetails {
    match_id: ByteArray,
    result_closure: PosixTime 
}

pub type BetDetails {
    bet_closure: PosixTime,
    bet_choice: ByteArray,
    bet_odds: Int,
    bet_amount: Int,
    bet_creator: VerificationKeyHash,
    bet_acceptor: VerificationKeyHash,
    bet_status: BetStatus
}

pub type BetDatum { 
  match: MatchDetails,
  bet: BetDetails
}

pub type ActionRedeemer {
  Accept
  Cancel
  Close
}

pub type BetRedeemer {
    action: ActionRedeemer,
    match: MatchDetails
}

pub type OracleDatum {
    match: MatchDetails,
    result: ByteArray
}

pub type OracleRedeemer {
  MintOracle
  BurnOracle
}