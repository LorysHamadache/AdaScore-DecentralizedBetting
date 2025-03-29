import { useState } from "react";
import React, { useEffect, useContext } from "react";
import { ToastContext } from "@/components/Utils/Toast";
import initLucid from "@/functions/chain/lucid";
import { useStoreState } from "@/functions/wallet-store";
import { readBetContractValidator } from "@/functions/chain/validators";
import {
	BetDatum,
	MatchDetails,
	BetDetails,
	AwaitingAcceptor,
	BetRedeemer,
	Action,
	TradeAction,
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

// Submit Data to Oracle
const Test = () => {
	const walletStore = useStoreState((state) => state.wallet);
	const [lucid, setLucid] = useState<Lucid | null>(null);
	const [TxHash, setTxHash] = useState("None");

	const { addToast } = useContext(ToastContext)!;

	function createToaster(mes: string, type: string) {
		addToast({ id: generateUniqueId(), message: mes, color: type });
	}

	useEffect(() => {
		if (!lucid && walletStore.connected) {
			initLucid(walletStore.name).then((Lucid: Lucid) => {
				setLucid(Lucid);
			});
		}
	}, [walletStore]);

	const initData = async () => {
		try {
			if (!lucid) {
				throw Error("Lucid not instantiated");
			}

			// Get Addresses
			const script_addr = lucid.utils.validatorToAddress(readBetContractValidator());
			const userPkh = lucid.utils.getAddressDetails(walletStore.address)
				.paymentCredential?.hash!;
			console.log("PKH")
			console.log(userPkh)

			const match_detail: MatchDetails = {
				match_id: fromText("Test"),
				match_result: fromText("Unknown"),
				result_closure: 4656546n,
			};
			const bet_detail = {
				bet_closure: 4656446n,
				bet_choice: fromText("fsdfsd"),
				bet_odds: 150n,
				bet_amount: 50_000_000n,
				bet_creator: userPkh,
				bet_acceptor: "",
				bet_status: AwaitingAcceptor,
			};

			const bet_datum: BetDatum = {
				match: match_detail,
				bet: bet_detail,
			};
			lucid.utils.validatorToScriptHash
			let tx = await lucid
				.newTx()
				.payToContract(
					script_addr,
					{ inline: Data.to<BetDatum>(bet_datum, BetDatum) },
					{ lovelace: 50_000_000n }
				)
				.complete();
			console.log(bet_datum);
			console.log(Data.to<BetDatum>(bet_datum, BetDatum));
			const signedtx = await tx.sign().complete();
			const txHash = signedtx.submit();
			console.log(txHash);
		} catch (e: any) {
			console.log(e);
			createToaster(e.toString(), "alert");
		}
	};

	const consumeData = async () => {
		try {
			if (!lucid) {
				throw Error("Lucid not instantiated");
			}

			// Get Addresses
			const script_addr = lucid.utils.validatorToAddress(script);
			const userPkh = lucid.utils.getAddressDetails(walletStore.address)
				.paymentCredential?.hash!;

			const utxo = (await lucid.provider.getUtxos(script_addr))[0];

			console.log(utxo);

			const match_detail: MatchDetails = {
				match_id: fromText("Test"),
				match_result: fromText("Unknown"),
				result_closure: 4656546n,
			};

			const my_act: Action = "Accept";
			const bet_redeemer = Data.to<BetRedeemer>(
				{
					action: "Cancel",
					match: match_detail,
				},
				BetRedeemer
			);

			let tx = await lucid
				.newTx()
				.attachSpendingValidator(script)
				.collectFrom([utxo], bet_redeemer)
				.complete();

			const signedtx = await tx.sign().complete();
			const txHash = signedtx.submit();
			console.log(txHash);
		} catch (e: any) {
			console.log(e);
			createToaster(e.toString(), "alert");
		}
	};

	return (
		<div>
			{" "}
			{lucid && walletStore.connected ? (
				<div className="flex-column justify-center text-center my-20">
					<div className="text-2xl my-20">Test Transaction buttons</div>
					<div className="text-lg my-6">
						Last Transaction Tx Hash: {TxHash}{" "}
					</div>
					<div className="flex items-center">
						<h2 className="">For Script 1</h2>
						<button className="nav-button px-12" onClick={initData}>
							Init
						</button>
						<button className="nav-button px-12" onClick={consumeData}>
							Consume
						</button>
					</div>
				</div>
			) : (
				<div>
					<div className="text-2xl my-20">
						{" "}
						Connect a Wallet to Initiate TXs
					</div>
				</div>
			)}
		</div>
	);
};

export default Test;
