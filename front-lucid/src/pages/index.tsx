import { Inter } from "next/font/google";

import Head from "next/head";
import Header from "@/components/Header/Header";
import FrontGallery from "@/components/Home/FrontGallery";
import MatchList from "@/components/Home/MatchList";
import LeftMenu from "@/components/Global/LeftMenu";
import Test from "@/components/Test";

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
			<main className="main">
				<div className="w-full flex flex-row gap-14">
					<LeftMenu />
					<div className="mainelement">
						<Test />
						<FrontGallery />
						<MatchList />
					</div>
				</div>
			</main>
		</>
	);
}
