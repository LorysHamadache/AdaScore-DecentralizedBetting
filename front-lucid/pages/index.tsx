import Head from "next/head";
import Header from "components/Header";
import MakeTransaction from "components/MakeTransaction";
import MyMenu from "components/MyMenu";
import FrontGallery from "@/components/HomeComponents/FrontGallery";
import Image from "next/image";
import { Inter } from "@next/font/google";
import MatchList from "@/components/HomeComponents/MatchList";

const inter = Inter({ subsets: ["latin"] });

export default function Home() {
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
						<FrontGallery />
						<MatchList />
					</div>
				</div>
			</main>
		</>
	);
}
