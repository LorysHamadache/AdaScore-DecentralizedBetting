import { SpendingValidator, Lucid, applyParamsToScript, } from "lucid-cardano";
import blueprint from "@/../../contracts/aiken/plutus.json" assert { type: "json" };
import initLucid from "./lucid";


const admin_pkh = "954714a58f5eb09639fefce6654ae921f53fbe16c0a4771b135b5f9d"

function readBetContractValidator(treasury_hash: string, oracle_policy: string): SpendingValidator {
	const script_code =
		blueprint.validators.find((v) => v.title === "bet_contract.bet_validator");

	if (!script_code) {
		throw new Error("Validator not found");
	}

	return {
		type: "PlutusV2",
		script: applyParamsToScript(script_code.compiledCode, [admin_pkh, treasury_hash, oracle_policy]),
	};
}

function readTreasuryValidator(): SpendingValidator {
	const script_code =
		blueprint.validators.find((v) => v.title === "treasury_contract.treasury_validator");

	if (!script_code) {
		throw new Error("Validator not found");
	}

	return {
		type: "PlutusV2",
		script: applyParamsToScript(script_code.compiledCode, [admin_pkh]),
	};
}

function readOraclePolicy(treasury_hash: string): SpendingValidator {
	const script_code =
		blueprint.validators.find((v) => v.title === "oracle_policy.mint_oracle");

	if (!script_code) {
		throw new Error("Validator not found");
	}
	return {
		type: "PlutusV2",
		script: applyParamsToScript(script_code.compiledCode, [admin_pkh, treasury_hash]),
	};
}

const lc = await Lucid.new()
export const treasury_validator = readTreasuryValidator();
const treasury_validator_pkh = lc.utils.validatorToScriptHash(treasury_validator)
export const oracle_policy = readOraclePolicy(treasury_validator_pkh)
const oracle_policy_hash = lc.utils.validatorToScriptHash(treasury_validator)
export const bet_contract_validator = readBetContractValidator(treasury_validator_pkh, oracle_policy_hash)
