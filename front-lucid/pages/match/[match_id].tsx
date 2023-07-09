import Head from "next/head";
import Header from "../../components/Header";
import MakeTransaction from "../../components/MakeTransaction";
import BetComponent from "../../components/MatchComponents/BetComponent";
import MatchHeader from "../../components/MatchComponents/MatchHeader";
import FrontGallery from "../../components/HomeComponents/FrontGallery";
import Image from "next/image";
import { Inter } from "@next/font/google";
import MatchList from "../../components/HomeComponents/MatchList";
import { CardanoWallet, useWallet } from "@meshsdk/react";
import React from "react";
import { useRouter } from "next/router";
import { useState, useEffect } from "react";
import { DocumentData } from "firebase/firestore";
import { getMatchDetails } from "../../src/dbcalls";
import { firestore_db } from "../../src/firebase-config";
import MyMenu from "components/MyMenu";

export default function MatchPage() {
	const router = useRouter();
	const [data, setData] = useState<DocumentData | null>(null);

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
	function handleClick() {
		if (data == null) {
			return "Error 404";
		} else {
			router.push(`/match/${data.match_id}/create`);
		}
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
					<div className="mainelement w-full">
						<MatchHeader data={data} />
						<div className="flex flex-row w-full justify-center">
							<button
								className="main-button font-semibold basis-2/6"
								onClick={handleClick}
							>
								CREATE
							</button>
						</div>
						<BetComponent />
					</div>
				</div>
			</main>
		</>
	);
}
