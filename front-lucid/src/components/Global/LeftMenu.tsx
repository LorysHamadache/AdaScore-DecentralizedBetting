import Image from "next/image";
import React from "react";
import { useState } from "react";
import { useRouter } from "next/router";
import {
	HomeIcon,
	WalletIcon,
	ChartBarIcon,
	ArrowPathIcon,
} from "@heroicons/react/24/solid";

const LeftMenu = () => {
	const router = useRouter();
	function handleClick(path: string) {
		router.push({ pathname: path });
	}

	const btnclassname = "menu-button flex h-12 my-1 ";
	const btnclassnameactive = "menu-button flex active h-12 my-1";
	console.log(router.pathname === "/" ? btnclassnameactive : btnclassname);
	return (
		<div className="mymenu flex flex-col card py-2 h-full ">
			<button
				className={router.pathname === "/" ? btnclassnameactive : btnclassname}
				onClick={() => handleClick("/")}
			>
				<HomeIcon className="headericons " />
				<p className="mx-4 text-lg"> Home </p>
			</button>
			<button
				className={
					router.pathname === "/bets" ? btnclassnameactive : btnclassname
				}
				onClick={() => handleClick("bets")}
			>
				<WalletIcon className="headericons" />
				<p className="mx-4 text-lg"> Bets </p>
			</button>
			<button
				className={
					router.pathname === "/stats" ? btnclassnameactive : btnclassname
				}
				onClick={() => handleClick("stats")}
			>
				<ChartBarIcon className="headericons" />
				<p className="mx-4 text-lg"> Stats </p>
			</button>
		</div>
	);
};

export default LeftMenu;
