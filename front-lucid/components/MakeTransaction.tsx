import { useWallet, CardanoWallet } from "@meshsdk/react";
import { BrowserWallet } from "@meshsdk/core";
import { Transaction, Asset } from "@meshsdk/core";
import type { Data } from "@meshsdk/core";

import React from "react";
import { script, scriptAddress } from "src/scriptconfig";
import { BlockfrostProvider } from "@meshsdk/core";
import { resolveDataHash, resolvePaymentKeyHash } from "@meshsdk/core";
import CreateBet from "@/components/MatchComponents/CreateBet";
import type { PlutusScript } from "@meshsdk/core";
import { firestore_functions } from "src/firebase-config";
import { httpsCallable } from "firebase/functions";

async function lockFunds(w: BrowserWallet) {
	const tx = new Transaction({ initiator: w }).sendLovelace(
		{ address: scriptAddress, datum: { value: "loryssecret" } },
		"10000000"
	);
	const unsignedTx = await tx.build();
	const signedTx = await w.signTx(unsignedTx);
	const txHash = await w.submitTx(signedTx);
	console.log(txHash);
}

async function unlockFunds(w: BrowserWallet) {
	const blockfrostProvider = new BlockfrostProvider(
		"previewik6GFwWXGMPRKns4XtVY34Of8WXBxTsN"
	);

	const utxos = await blockfrostProvider.fetchAddressUTxOs(scriptAddress);
	const dataHash = resolveDataHash("loryssecret");
	let utxo = utxos.find((utxo: any) => {
		return utxo.output.dataHash == dataHash;
	});

	const tx = new Transaction({ initiator: w })
		.redeemValue({ value: utxo, script: script, datum: "loryssecret" })
		.sendValue(
			"addr_test1qz8t4dnghur8s4khdvun57w7y3gd5duaq9g0s0m6r5nck5qxyqqnl8snr286qw6hrgmxvu483zaa52edrkksqedcmg6ssk4e5z",
			utxo
		)
		.setRequiredSigners([
			"addr_test1qz8t4dnghur8s4khdvun57w7y3gd5duaq9g0s0m6r5nck5qxyqqnl8snr286qw6hrgmxvu483zaa52edrkksqedcmg6ssk4e5z",
		]);
	const unsignedTx = await tx.build();
	const signedTx = await w.signTx(unsignedTx, true);
	const txHash = await w.submitTx(signedTx);
	console.log(txHash);
}

async function createReferenceScript(w: BrowserWallet) {
	const addr = await w.getUsedAddress();
	const pkh = resolvePaymentKeyHash(addr.to_js_value());
	const datumRefScript: Data = {
		alternative: 0,
		fields: [
			"0", //matchid
			1676343437000, //closed_at
			1676343438000, //resultlimAt
			{ alternative: 3, fields: [] }, //result (0-Win, 1-Draw, 2-Loss, 3-Unknown)
			{ alternative: 0, fields: [] }, //creatorbet
			150, //d_odds,
			50000000, //d_amount
			2500000, //d_fee
			pkh, //creator_pkh
			pkh, //acceptor_pkh
			{ alternative: 0, fields: [] }, //d_status
		],
	};
	const tx = new Transaction({ initiator: w }).sendLovelace(
		{
			address: scriptAddress,
			datum: {
				value: datumRefScript,
				inline: true,
			},
			script: script,
		},
		"52500000"
	);
	const unsignedTx = await tx.build();
	const signedTx = await w.signTx(unsignedTx);
	const txHash = await w.submitTx(signedTx);
	console.log(txHash);

	const firebase_datum = {
		accept_txhash: "",
		bet_txhash: txHash,
		cancel_txhash: "",
		close_txhash: "",
		creator_ref: addr.to_js_value(),
		acceptor_ref: null,
		date: null,
		datum_acceptor_pkh: pkh,
		datum_closed_at: 1676343437000,
		datum_creator_pkh: pkh,
		datum_creatorbet: 0,
		datum_d_amount: 50000000,
		datum_d_fee: 2500000,
		datum_d_odds: 150,
		datum_d_status: "AwaitingBet",
		datum_matchid: "FIdzHccSOkoGjUOAXpOO",
		datum_result: 3,
		datum_result_at: 1676343438000,
		match_ref: null,
		status: "",
		updateddate: null,
	};
	const createBetCallable = httpsCallable(
		firestore_functions,
		"createRefScript"
	);
	createBetCallable(firebase_datum)
		.then((result: any) => {
			console.log(result.data);
		})
		.catch((error: any) => {
			console.error(error);
		});
}

async function createReferenceScript2(w: BrowserWallet) {
	const addr = await w.getUsedAddress();
	const pkh = resolvePaymentKeyHash(addr.to_js_value());

	const tx = new Transaction({ initiator: w }).sendLovelace(
		{
			address:
				"addr_test1qp298ux4axxgsud5ruu7tj2mhuc2gwv8ylm8y7r4ja9ezy9lpdrvx4x8x5ns6gdwlsxpvrg53yeydc08a3qaq2uwatjqqfnn8w",
			script: script,
		},
		"99500000"
	);
	const unsignedTx = await tx.build();
	const signedTx = await w.signTx(unsignedTx);
	const txHash = await w.submitTx(signedTx);
	console.log(txHash);
}

const MakeTransaction = () => {
	const { connected, wallet } = useWallet();
	return (
		<div>
			<div className="px-4 py-4 mb-20">
				<p>Bet Creation</p>
				<button className="nav-button" onClick={() => lockFunds(wallet)}>
					Create Bet
				</button>
			</div>
			<div className="flex justify-between items-center md:p-3">
				<button className="nav-button" onClick={() => unlockFunds(wallet)}>
					Accept Bet
				</button>
				<button className="nav-button">Cancel Bet</button>
				<button className="nav-button">Close Bet</button>
			</div>
			<div className="flex justify-between items-center md:p-4">
				<button className="nav-button">Create Ref Oracle</button>
				<button
					className="nav-button"
					onClick={() => createReferenceScript2(wallet)}
				>
					Create Ref Script
				</button>
			</div>
			<CreateBet></CreateBet>
		</div>
	);
};

export default MakeTransaction;
