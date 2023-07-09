import Head from "next/head";
import Header from "../../../components/Header";
import MakeTransaction from "../../../components/MakeTransaction";
import MatchHeader from "../../../components/MatchComponents/MatchHeader";
import BetComponent from "../../../components/MatchComponents/BetComponent";
import FrontGallery from "../../../components/HomeComponents/FrontGallery";
import Image from "next/image";
import { Inter } from "@next/font/google";
import MatchList from "../../../components/HomeComponents/MatchList";
import { CardanoWallet, useWallet } from "@meshsdk/react";
import React from "react";
import { useRouter } from "next/router";
import { useState, useEffect } from "react";
import { DocumentData } from "firebase/firestore";
import { getMatchDetails } from "../../../src/dbcalls";
import { firestore_db } from "../../../src/firebase-config";
import CreateBet from "@/components/MatchComponents/CreateBet";
import MyMenu from "components/MyMenu";

const inter = Inter({ subsets: ["latin"] });

export default function CreateBetPage() {
	const router = useRouter();
	const [data, setData] = useState<DocumentData | null>(null);
	const [refreshCount, setRefreshCount] = useState(0);

	useEffect(() => {
		const fetchMatchDetails = async () => {
			const match_id =
				typeof router.query.match_id == "string"
					? router.query.match_id
					: "undefined";
			const match_query = await getMatchDetails(firestore_db, match_id);
			const match_data = match_query.exists()
				? { match_id: match_query.id, ...match_query.data() }
				: null;
			setData(match_data);
		};
		fetchMatchDetails();
	}, [router.query]);
	if (data == null) {
		return "Error 404";
	}
	return (
		<>
			<Head>
				<title>AdaScore</title>
				<meta
					name="description"
					content="AdaScore - The Cardano Decentralized Betting Platform"
				/>
				<meta name="viewport" content="width=device-width, initial-scale=1" />
				<link rel="icon" href="/favicon.ico" />
			</Head>
			<Header />
			<main className="main border">
				<div className="w-full flex flex-row gap-14">
					<MyMenu />
					<div className="mainelement">
						<MatchHeader data={data} />
						<CreateBet data={data} />
					</div>
				</div>
			</main>
		</>
	);
}

/*
<main className="main">
<div className="w-full flex flex-col">
	<MatchHeader data={data} />
	<CreateBet data={data} />
</div>
</main>*/
