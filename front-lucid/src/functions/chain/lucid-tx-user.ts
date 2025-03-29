import { useState } from "react";
import React, { useEffect, useContext } from "react";
import { ToastContext } from "@/components/Utils/Toast";
import initLucid from "@/functions/chain/lucid";
import { useStoreState } from "@/functions/wallet-store";
import { bet_contract_validator } from "@/functions/chain/validators";
import {
    BetDatum,
    BetDetails,
    MatchDetails
} from "@/functions/chain/lucid-types";

import {
    Blockfrost,
    Lucid,
    Network,
    Assets,
    toUnit,
    Constr,
    Data,
    fromText,
    Redeemer,
    applyParamsToScript,
    MintingPolicy,
} from "lucid-cardano";

import { generateUniqueId } from "@/functions/utils";

const initBet = async (lucid: Lucid, match_detail: MatchDetails, bet_detail: BetDetails) => {
    if (!lucid) {
        throw Error("Lucid not instantiated");
    }
    const bet_contract_addr = lucid.utils.validatorToAddress(bet_contract_validator);

    const bet_datum: BetDatum = {
        match: match_detail,
        bet: bet_detail,
    };

    lucid.utils.validatorToScriptHash
    let tx = await lucid
        .newTx()
        .payToContract(
            bet_contract_addr,
            { inline: Data.to<BetDatum>(bet_datum, BetDatum) },
            { lovelace: bet_detail.bet_amount + minfee_calculation(bet_detail.bet_amount) }
        )
        .complete();
    const signedtx = await tx.sign().complete();
    const txHash = signedtx.submit();
    console.log(txHash);

};


const acceptBet = async (lucid: Lucid, match_detail: MatchDetails, bet_detail: BetDetails) => {
    if (!lucid) {
        throw Error("Lucid not instantiated");
    }
    const bet_contract_addr = lucid.utils.validatorToAddress(bet_contract_validator);

    const bet_datum: BetDatum = {
        match: match_detail,
        bet: bet_detail,
    };

    lucid.utils.validatorToScriptHash
    let tx = await lucid
        .newTx()
        .payToContract(
            bet_contract_addr,
            { inline: Data.to<BetDatum>(bet_datum, BetDatum) },
            { lovelace: bet_detail.bet_amount + minfee_calculation(bet_detail.bet_amount) }
        )
        .complete();
    const signedtx = await tx.sign().complete();
    const txHash = signedtx.submit();
    console.log(txHash);

};





const minfee_calculation = (bet_amount: bigint): bigint => {
    // Convert environment variables to bigint
    const SERVICEMINFEE = BigInt(process.env.SERVICEMINFEE!);
    const SERVICEPERCFEE = BigInt(process.env.SERVICEPERCFEE!);

    // Calculate percentage fee
    const percentageFee = bet_amount * SERVICEPERCFEE / BigInt(100);

    // Return the maximum between SERVICEMINFEE and percentageFee
    return SERVICEMINFEE > percentageFee ? SERVICEMINFEE : percentageFee;
}