import { Constr, Data } from "lucid-cardano";

// BetStatus
const BetStatusSchema = Data.Enum([
	Data.Literal("AwaitingAcceptor"),
	Data.Literal("AwaitingResult"),
	Data.Literal("Closed"),
]);
export type BetStatus = Data.Static<typeof BetStatusSchema>;
export const BetStatus = BetStatusSchema as unknown as BetStatus;

// MatchDetails
const MatchDetailsSchema = Data.Object({
	match_id: Data.Bytes(),
	result_closure: Data.Integer(),
});
export type MatchDetails = Data.Static<typeof MatchDetailsSchema>;
export const MatchDetails = MatchDetailsSchema as unknown as MatchDetails;

// BetDetails
const BetDetailsSchema = Data.Object({
	bet_closure: Data.Integer(),
	bet_choice: Data.Bytes(),
	bet_odds: Data.Integer(),
	bet_amount: Data.Integer(),
	bet_creator: Data.Bytes(),
	bet_acceptor: Data.Bytes(),
	bet_status: BetStatusSchema,
});
export type BetDetails = Data.Static<typeof BetDetailsSchema>;
export const BetDetails = BetDetailsSchema as unknown as BetDetails;

// BetDatum
const BetDatumSchema = Data.Object({
	match: MatchDetailsSchema,
	bet: BetDetailsSchema,
});
export type BetDatum = Data.Static<typeof BetDatumSchema>;
export const BetDatum = BetDatumSchema as unknown as BetDatum;

// Action
const ActionSchema = Data.Enum([
	Data.Literal("Accept"),
	Data.Literal("Cancel"),
	Data.Literal("Close"),
]);
export type Action = Data.Static<typeof ActionSchema>;
export const Action = ActionSchema as unknown as Action;

// Bet Redeemer
const BetRedeemerSchema = Data.Object({
	action: ActionSchema,
	match: MatchDetailsSchema,
});
export type BetRedeemer = Data.Static<typeof BetRedeemerSchema>;
export const BetRedeemer = BetRedeemerSchema as unknown as BetRedeemer;

//Oracle Datum
const OracleDatumSchema = Data.Object({
	match: MatchDetailsSchema,
	result: Data.Bytes(),
});
export type OracleDatum = Data.Static<typeof OracleDatumSchema>;
export const OracleDatum = OracleDatumSchema as unknown as OracleDatum;

//Oracle Redeemer
const OracleRedeemerSchema = Data.Enum([
	Data.Literal("MintOracle"),
	Data.Literal("BurnOracle"),
]);
export type OracleRedeemer = Data.Static<typeof OracleRedeemerSchema>;
export const OracleRedeemer = OracleRedeemerSchema as unknown as OracleRedeemer;
