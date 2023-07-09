import { DocumentData } from "@firebase/firestore";
import React from "react";
import { useState } from "react";
import { showDate } from "../../src/utils";
import { type } from "os";
import { BrowserWallet, resolvePaymentKeyHash } from "@meshsdk/core";
import { CardanoWallet, useWallet } from "@meshsdk/react";
import { Transaction, Asset } from "@meshsdk/core";
import type { Data } from "@meshsdk/core";
import { scriptAddress } from "@/src/scriptconfig";
import { firestore_db, firestore_functions } from "src/firebase-config";
import { httpsCallable } from "firebase/functions";

const CreateBet = (props: DocumentData) => {
	const { connected, wallet } = useWallet();
	const { data }: DocumentData = props;
	const [odds, setOdds] = useState(1.5);
	const [selectedOutput, setselectedOutput] = useState(0);
	const [selectedAmount, setselectedAmount] = useState(100);
	const [selectedAmountAda, setselectedAmountAda] = useState("100 ₳");

	const handleOddsChange = (event: any) => {
		setOdds(event.target.value);
	};

	const handleOddsBlur = (event: any) => {
		const newValue = parseFloat(event.target.value);
		if (newValue >= 1.01 && newValue <= 9.99) {
			setOdds(parseFloat(newValue.toFixed(2)));
		} else {
			setOdds(1.5);
		}
	};

	const handleOutputClick = (tabIndex: number) => {
		setselectedOutput(tabIndex);
		console.log(selectedOutput);
	};

	const handleAmountChange = (event: any) => {
		setselectedAmount(event.target.value);
		setselectedAmountAda(event.target.value);
	};

	const handleAmountBlur = (event: any) => {
		const newValue = parseInt(event.target.value);
		if (newValue >= 5) {
			let x = parseFloat(newValue.toFixed(0));
			setselectedAmount(x);
			setselectedAmountAda(x + " ₳");
		} else if (newValue <= 5) {
			setselectedAmount(5);
			setselectedAmountAda(5 + " ₳");
		} else {
			setselectedAmount(100);
			setselectedAmountAda(100 + " ₳");
		}
	};

	const teamBet = () => {
		if (selectedOutput == 0) {
			return data.team_a;
		} else if (selectedOutput == 2) {
			return data.team_b;
		} else return " a Draw";
	};

	const feeCalculation = () => {
		const fee = 0.05 * selectedAmount;
		if (fee >= 5) {
			return fee;
		} else {
			return 2;
		}
	};

	const potentialGains = () => {
		return (selectedAmount * odds - feeCalculation()).toFixed(0);
	};
	if (!props || !data) {
		return null;
	}
	return (
		<div className="flex flex-row w-full justify-center">
			<div className="justify-top flex flex-col basis-3/6 card px-5 my-10 mx-5">
				<h1 className="text-center font-extrabold tracking-tight text-gray-800 md:text-2xl lg:text-3xl text-white my-8">
					Create your bet
				</h1>
				<form>
					<ChooseBetForm
						data={data}
						selectedOutput={selectedOutput}
						setSelectedOutput={setselectedOutput}
						handleOutputClick={handleOutputClick}
					/>
					<ChooseOddsForm
						odds={odds}
						handleOddsBlur={handleOddsBlur}
						handleOddsChange={handleOddsChange}
					/>
					<ChooseAmountForm
						selectedAmount={selectedAmount}
						selectedAmountAda={selectedAmountAda}
						handleAmountBlur={handleAmountBlur}
						handleAmountChange={handleAmountChange}
					/>
					<br />
				</form>
			</div>

			<div className="justify-top flex flex-col basis-2/6 card px-5 my-10">
				<h1 className="text-center font-extrabold tracking-tight text-gray-800 md:text-2xl lg:text-3xl text-white my-8">
					Details
				</h1>
				<div className="font-bold py-2">
					<label className="card_lowertitle text-left">The Match</label>
					<p>
						{data.team_a} - {data.team_b}
					</p>
					<p>{showDate(data.date)}</p>
				</div>
				<div className="py-2">
					<label className="card_lowertitle text-left ">Considerations</label>
					<p>
						<b>Acceptance Window End:</b> {showDate(data.closed_at)}{" "}
					</p>
					<p>
						<b>Closing Window End:</b> {showDate(data.resultlim_at)}{" "}
					</p>
				</div>
				<div>
					<label className="card_lowertitle text-left py-2">Your Bet</label>
					<p>
						You have bet on <span className="accentuated">{teamBet()}</span>{" "}
						with odds of <span className="accentuated">{odds}</span> and the
						following amount of{" "}
						<span className="accentuated">
							{selectedAmount} ₳ + {feeCalculation()} ₳ Fee
						</span>
					</p>
				</div>
				<div className="font-extrabold text-center py-4 text-xl">
					If your bet is right, you will earn
					<p className="text-2xl"> {potentialGains()} ₳</p>
				</div>
				<button
					className="text-xl main-button my-5 font-semibold w-full justify-center"
					type="button"
					onClick={() =>
						CreateBetTransaction({
							selectedAmount,
							odds,
							selectedOutput,
							wallet,
							data,
							fee: feeCalculation(),
						})
					}
				>
					CREATE BET
				</button>
			</div>
		</div>
	);
};
export default CreateBet;

function ChooseBetForm({
	data,
	selectedOutput,
	setSelectedOutput,
	handleOutputClick,
}: {
	data: DocumentData;
	selectedOutput: any;
	setSelectedOutput: any;
	handleOutputClick: any;
}) {
	return (
		<div className="flex flex-col">
			<label className="card_lowertitle text-center">Choose Bet</label>
			<div className="flex justify-center space-x-4">
				<label
					className={`cursor-pointer nav-button ${
						selectedOutput === 0 ? "selected-choice" : ""
					}`}
				>
					{data.team_a} - Win
					<input
						type="radio"
						name="selector"
						value="win"
						className="sr-only"
						onClick={() => handleOutputClick(0)}
					/>
				</label>
				<label
					className={`cursor-pointer nav-button ${
						selectedOutput === 1 ? "selected-choice" : ""
					}`}
				>
					Draw
					<input
						type="radio"
						name="selector"
						value="draw"
						className="sr-only"
						onClick={() => handleOutputClick(1)}
					/>
				</label>
				<label
					className={`cursor-pointer nav-button ${
						selectedOutput === 2 ? "selected-choice" : ""
					}`}
				>
					{data.team_b} - Win
					<input
						type="radio"
						name="selector"
						value="loss"
						className="sr-only"
						onClick={() => handleOutputClick(2)}
					/>
				</label>
			</div>
		</div>
	);
}

function ChooseOddsForm({
	handleOddsChange,
	handleOddsBlur,
	odds,
}: {
	handleOddsChange: any;
	handleOddsBlur: any;
	odds: number;
}) {
	return (
		<div className="flex flex-col">
			<label className="card_lowertitle text-center">Choose Odds</label>
			<input
				type="text"
				value={odds}
				onChange={handleOddsChange}
				min="1.01"
				max="9.99"
				step="0.01"
				onBlur={handleOddsBlur}
				className="textbox-def rounded-full font-bold text-center md:text-lg lg:text-xl"
			/>
			<input
				type="range"
				min="1.01"
				max="9.99"
				step="0.01"
				value={odds}
				onChange={handleOddsChange}
				className="card_slider mx-5 my-5 bg-gray-300 appearance-none rounded-full h-4  cursor-pointer"
			/>
		</div>
	);
}

function ChooseAmountForm({
	selectedAmount,
	selectedAmountAda,
	handleAmountBlur,
	handleAmountChange,
}: {
	selectedAmount: number;
	selectedAmountAda: string;
	handleAmountBlur: any;
	handleAmountChange: any;
}) {
	return (
		<div className="flex flex-col">
			<label className="card_lowertitle text-center">Choose Amount</label>
			<input
				type="text"
				value={selectedAmountAda}
				onChange={handleAmountChange}
				min="5"
				max="5000"
				step="1"
				onBlur={handleAmountBlur}
				className="textbox-def rounded-full font-bold text-center md:text-lg lg:text-xl"
			/>
		</div>
	);
}

async function CreateBetTransaction({
	selectedAmount,
	odds,
	selectedOutput,
	wallet,
	data,
	fee,
}: {
	selectedAmount: number;
	odds: number;
	selectedOutput: number;
	wallet: BrowserWallet;
	data: DocumentData;
	fee: number;
}) {
	const addr = await wallet.getUsedAddress();
	const pkh = resolvePaymentKeyHash(addr.to_js_value());
	const datumtx: Data = {
		alternative: 0,
		fields: [
			data.match_id, //matchid
			data.closed_at.toMillis(), //closed_at
			data.resultlim_at.toMillis(), //resultlimAt
			{ alternative: 3, fields: [] }, //result (0-Win, 1-Draw, 2-Loss, 3-Unknown)
			{ alternative: selectedOutput, fields: [] }, //creatorbet
			odds * 100, //d_odds,
			selectedAmount * 1000000, //d_amount
			fee * 1000000, //d_fee
			pkh, //creator_pkh
			pkh, //acceptor_pkh
			{ alternative: 0, fields: [] }, //d_status
		],
	};

	const tx = new Transaction({ initiator: wallet }).sendLovelace(
		{
			address: scriptAddress,
			datum: {
				value: datumtx,
				inline: true,
			},
		},
		((selectedAmount + fee) * 1000000).toString()
	);
	const unsignedTx = await tx.build();
	const signedTx = await wallet.signTx(unsignedTx);
	const txHash = await wallet.submitTx(signedTx);
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
		datum_closed_at: data.closed_at.toMillis(),
		datum_creator_pkh: pkh,
		datum_creatorbet: selectedOutput,
		datum_d_amount: selectedAmount * 1000000,
		datum_d_fee: fee * 1000000,
		datum_d_odds: odds * 100,
		datum_d_status: "AwaitingBet",
		datum_matchid: data.match_id,
		datum_result: 3,
		datum_result_at: data.resultlim_at.toMillis(),
		match_ref: null,
		status: "",
		updateddate: null,
	};
	const createBetCallable = httpsCallable(firestore_functions, "createBet");
	createBetCallable(firebase_datum)
		.then((result) => {
			console.log(result.data);
		})
		.catch((error) => {
			console.error(error);
		});
}
