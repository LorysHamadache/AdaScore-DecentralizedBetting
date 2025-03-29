import Head from "next/head";
import { useRouter } from "next/router";
import React, { useState } from "react";
import { Inter } from "next/font/google";
import { DocumentData } from "firebase/firestore";

import Header from "@/components/Header/Header";
import LeftMenu from "@/components/Global/LeftMenu";
import PageTitle from "@/components/Global/PageTitle";

import UserBets from "@/components/Profile/UserBets";

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
					<LeftMenu />
					<div className="mainelement">
						<PageTitle Title={title} />
						<UserBets />
					</div>
				</div>
			</main>
		</>
	);
}
