import Image from "next/image";

import Router from "next/router";
import React from "react";
import { firestore_db } from "../../src/firebase-config";
import { getMyBetsCreator } from "../../src/dbcalls";
import { useState, useEffect } from "react";
import { DocumentData } from "firebase/firestore";
import { ArrowPathIcon } from "@heroicons/react/20/solid";
import { useRouter } from "next/router";
import { Url } from "url";
import { showDate } from "../../src/utils";
import { scriptAddress, ref_scriptUtxo, script } from "@/src/scriptconfig";
import { httpsCallable } from "firebase/functions";
import { firestore_functions } from "src/firebase-config";
import {
	fetchScriptUtxo,
	fetchRefScriptUtxo,
	fetchRefScriptUtxo2,
} from "@/src/blockfrost";

const MyBetsList = () => {
	const { connected, wallet } = useWallet();
	const [data_betCreator, setDataBetCreator] = useState<DocumentData[]>([]);
	const [refreshCount, setRefreshCount] = useState(0);

	const handleRefresh = () => {
		setRefreshCount((prevCount) => prevCount + 1);
	};

	useEffect(() => {
		const fetchBetsAcceptor = async () => {
			const addr = await wallet.getUsedAddress();
			const bets_acceptor_query = await getMyBetsCreator(
				firestore_db,
				addr.to_js_value()
			);
			setDataBetCreator(bets_acceptor_query);
		};

		if (connected) {
			fetchBetsAcceptor();
		}
	}, [refreshCount, connected]);

	const router = useRouter();
	function handleClick(path: string) {
		router.push({ pathname: path });
	}

	function getBetValueAcceptor(bet: DocumentData) {
		const choice = bet.datum_creatorbet;
		switch (choice) {
			case 0:
				return bet.match_data.team_a;
			case 1:
				return "Draw";
			case 2:
				return bet.match_data.team_b;
			default:
				throw new Error("Invalid Choice");
		}
	}

	function getResultforDatum(res: string) {
		switch (res) {
			case "Win":
				return 0;
			case "Draw":
				return 1;
			case "Lose":
				return 2;
			case "Unknown":
				return 3;
			default:
				throw new Error("Invalid Result");
		}
	}

	return (
		<div className="w-full flex flex-col justify-center">
			<div className="flex my-2 items-center">
				<button
					onClick={handleRefresh}
					className="h-9 nav-button flex items-center"
				>
					<ArrowPathIcon className="headericons "></ArrowPathIcon>
				</button>
			</div>
			<table className="table-match basis-7/12 border-collapse">
				<tbody>
					{data_betCreator.map((bet) => (
						<tr key={bet.id} className="row-match flex w-full items-center">
							<td
								rowSpan={2}
								className="basis-2/6 md:text-xl lg:text-2xl font-semibold"
							>
								<p>
									{bet.match_data.team_a} - {bet.match_data.team_b}
								</p>
								<p className="md:text-xs lg:text-sm font-regular">
									{showDate(bet.match_data.date)}
								</p>
							</td>
							<td className="basis-2/6 md:text-lg lg:text-xl font-semibold text-center">
								<p>{getBetValueAcceptor(bet)}</p>
							</td>
							<td className="basis-2/6 md:text-lg lg:text-xl font-semibold text-center">
								<p>{bet.datum_d_amount / 1000000} ₳ </p>
							</td>
							<td className="basis-2/6 md:text-lg lg:text-xl font-semibold text-center">
								<p>x{bet.datum_d_odds / 100}</p>
							</td>
							<td className="basis-2/6 md:text-lg lg:text-xl font-semibold text-center">
								{bet.status}
							</td>
							<td className="flex flex-grow basis-2/6 text-center">
								<button
									className="nav-button font-semibold flex-1"
									onClick={() => true}
								>
									Close
								</button>
								<button
									className="nav-button font-semibold flex-1"
									onClick={() =>
										CancelBetTransaction({ wallet, bet_data: bet })
									}
								>
									Cancel
								</button>
							</td>
						</tr>
					))}
				</tbody>
			</table>
		</div>
	);
};
export default MyBetsList;

async function CancelBetTransaction({
	wallet,
	bet_data,
}: {
	wallet: BrowserWallet;
	bet_data: DocumentData;
}) {
	const addr = await wallet.getUsedAddress();
	const datumtx: Data = firebaseDatumToDatum(bet_data);
	const redeemertx: Partial<Action> = {
		data: {
			alternative: 0,
			fields: [bet_data.datum_matchid, bet_data.datum_creator_pkh, "cancel"],
		},
	};
	const utxo: UTxO = await fetchScriptUtxo(bet_data.bet_txhash);
	const fetched_refutxo: UTxO = await fetchRefScriptUtxo();
	console.log(addr.to_js_value());
	console.log(utxo);
	console.log(datumtx);
	console.log(redeemertx);
	console.log(fetched_refutxo);
	const tx = new Transaction({ initiator: wallet })
		.redeemValue({
			value: utxo,
			datum: utxo,
			redeemer: redeemertx,
			script: fetched_refutxo,
		})
		.sendValue({ address: addr.to_js_value() }, utxo)
		.setRequiredSigners([addr.to_js_value()]);

	let minutes = 5; // add 5 minutes
	let nowDateTime = new Date();
	let dateTimeAdd5Min = new Date(nowDateTime.getTime() + minutes * 60000);
	const slot = resolveSlotNo("preprod", dateTimeAdd5Min.getTime());
	tx.setTimeToExpire(slot);
	const unsignedTx = await tx.build();
	console.log(unsignedTx);
	const signedTx = await wallet.signTx(unsignedTx, true);
	console.log("Signed TX");
	console.log(signedTx);
	try {
		const txHash = await wallet.submitTx(signedTx);
		console.log("Hello", txHash);
		const createBetCallable = httpsCallable(firestore_functions, "cancelBet");
		createBetCallable({ documentId: bet_data.id, txHash: txHash })
			.then((result: any) => {
				console.log(result.data);
			})
			.catch((error: any) => {
				console.error(error);
			});
	} catch (error) {
		console.log("Error 2 ");
		console.log(error);
	}
}

function firebaseDatumToDatum(firebase_datum: DocumentData) {
	console.log(firebase_datum.datum_closed_at);
	const datumtx: Data = {
		alternative: 0,
		fields: [
			firebase_datum.datum_matchid,
			firebase_datum.datum_closed_at,
			firebase_datum.datum_result_at,
			{
				alternative: firebase_datum.datum_result,
				fields: [],
			},
			{ alternative: firebase_datum.datum_creatorbet, fields: [] },
			firebase_datum.datum_d_odds,
			firebase_datum.datum_d_amount,
			firebase_datum.datum_d_fee,
			firebase_datum.datum_creator_pkh,
			firebase_datum.datum_acceptor_pkh,
			{ alternative: 0, fields: [] },
		],
	};
	return datumtx;
}
