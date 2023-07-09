import Image from "next/image";
import { useState } from "react";
import Router from "next/router";
import {
	Cog6ToothIcon,
	ChatBubbleBottomCenterTextIcon,
	AcademicCapIcon,
} from "@heroicons/react/24/solid";
import axios from "axios";
import { useEffect, useContext } from "react";
import WalletConnect from "./WalletConnect";

const Header = () => {
	const url =
		"https://europe-west1-project-adascorepreprod.cloudfunctions.net/updateUser";

	return (
		<header className="flex justify-between items-center md:p-4">
			<Image
				src="/Logo.png"
				alt="Logo"
				className="logo"
				width={250}
				height={100}
				priority
			/>
			<div className="flex">
				<WalletConnect />
				<button className="nav-button">
					<AcademicCapIcon className="headericons" />
				</button>
				<button className="nav-button">
					<ChatBubbleBottomCenterTextIcon className="headericons" />
				</button>
				<button className="nav-button">
					<Cog6ToothIcon className="headericons" />
				</button>
			</div>
		</header>
	);
};

export default Header;
