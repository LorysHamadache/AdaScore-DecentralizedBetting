import Head from "next/head";
import Header from "../components/Header";
import MyMenu from "components/MyMenu";
import PageTitle from "components/PageTitle";
import MakeTransaction from "../components/MakeTransaction";
import BetComponent from "../components/MatchComponents/BetComponent";
import MatchHeader from "../components/MatchComponents/MatchHeader";
import FrontGallery from "../components/HomeComponents/FrontGallery";
import Image from "next/image";
import { Inter } from "@next/font/google";
import MyBetsList from "../components/MyBetsComponents/MyBetsList";
import React from "react";
import { useRouter } from "next/router";
import { useState, useEffect } from "react";
import { DocumentData } from "firebase/firestore";
import { getMatchDetails } from "../src/dbcalls";
import { firestore_db } from "../src/firebase-config";

const inter = Inter({ subsets: ["latin"] });

export default function MatchPage() {
	const title: string = "My Bets";
	const router = useRouter();
	const [data, setData] = useState<DocumentData | null>(null);

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
			<main className="main ">
				<div className="w-full flex flex-row gap-14 ">
					<MyMenu />
					<div className="mainelement">
						<PageTitle Title={title} />
						<MyBetsList />
					</div>
				</div>
			</main>
		</>
	);
}
