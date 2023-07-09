import Image from "next/image";
import { useState } from "react";
import Router from "next/router";
import React, { useEffect, useContext } from "react";
import { ToastContext } from "./UtilsComponents/Toast";
import initLucid from "../src/lucid";
import { getBalance } from "../src/lucid";
import { Lucid, Blockfrost } from "lucid-cardano";
import { useStoreActions, useStoreState } from "../src/store";
import Spinner from "./UtilsComponents/Spinner";
import { shortAddress, generateUniqueId } from "../src/utils";

const WalletConnect = () => {
	//Wallet
	const [ishovered, updatehover] = useState(false);
	const [loading, setloading] = useState(true);

	//Lucid
	const [lucid, setLucid] = useState<Lucid>(null);
	const [api, setApi] = useState(null);
	const [balance, setBalance] = useState<string>("");

	// WalletStore Init
	const walletStore = useStoreState((state) => state.wallet);
	const setWallet = useStoreActions((actions) => actions.setWallet);
	const availableWallets = useStoreState((state) => state.availableWallets);
	const setAvailableWallets = useStoreActions(
		(actions) => actions.setAvailableWallets
	);

	// Wallet Connect Variable
	const accepted_wallets = ["Eternl", "Nami", "Flint"];
	// Toaster
	const { addToast } = useContext(ToastContext)!;

	function createToaster(mes, type) {
		addToast({ id: generateUniqueId(), message: mes, color: type });
	}

	useEffect(() => {
		let wallets = [];
		if (window.cardano) {
			if (window.cardano.nami) wallets.push("Nami");
			if (window.cardano.flint) wallets.push("Flint");
			if (window.cardano.eternl) wallets.push("Eternl");
		}
		setAvailableWallets(wallets);
		setloading(false);
	}, [lucid, walletStore, balance, api]);

	useEffect(() => {
		loadWalletSession();
	}, []);

	const connectWallet = async (wallet: string) => {
		setloading(true);
		const newapi = await window.cardano[wallet.toLowerCase()].enable();
		setApi(newapi);

		if (window.cardano && newapi) {
			if (
				(await newapi.getNetworkId()) != process.env.NEXT_PUBLIC_CARDANOENVNB
			) {
				createToaster("Error: Wrong Network - Disconnected", "alert");
				const walletStoreObj = { connected: false, name: "", address: "" };
				setloading(false);
				-setApi(null);
				return null;
			}
			const newlucid = await initLucid(wallet);
			setLucid(newlucid);
			const addr = await newlucid.wallet.address();
			const walletStoreObj = { connected: true, name: wallet, address: addr };
			setWallet(walletStoreObj);
			const lvbalance: string = await getBalance(newlucid, addr);
			const fbalance = (lvbalance / 1000000n).toString();
			setBalance(fbalance);
			createToaster("Connected: " + shortAddress(addr), "info");
		} else {
			const walletStoreObj = { connected: false, name: "", address: "" };
			setWallet(walletStoreObj);
			setloading(false);
			setApi(null);
			createToaster("Disconnected", "alert");
		}
	};

	const diconnectWallet = async () => {
		setloading(true);
		if (
			window.cardano &&
			(await window.cardano[walletStore.name.toLowerCase()].enable()) &&
			walletStore.connected
		) {
			const walletStoreObj = { connected: false, name: "", address: "" };
			setBalance("");
			setWallet(walletStoreObj);
			createToaster("Disconnected", "alert");
		}
	};

	const loadWalletSession = async () => {
		if (
			walletStore.connected &&
			walletStore.name &&
			window.cardano &&
			(await window.cardano[walletStore.name.toLowerCase()].enable())
		) {
			connectWallet(walletStore.name);
		}
	};

	if (loading) {
		return (
			<div className="w-44 h-12 wallet-button flex items-center justify-center relative ">
				<Spinner />
			</div>
		);
	}
	if (walletStore.connected) {
		return (
			<div
				className="w-44 h-12 wallet-button flex items-center justify-center relative "
				onMouseEnter={() => updatehover(true)}
				onMouseLeave={() => updatehover(false)}
			>
				<img
					className="mx-1 "
					src={`/wallet-icons/${walletStore.name.toLowerCase()}.svg`}
					height="30"
					width="30"
				></img>
				<p className="mx-2 text-lg"> ₳ {balance} </p>
				{ishovered ? (
					<ul className="absolute w-full top-9 wallet-list">
						<li
							className="flex w-full justify-center wallet-item"
							onClick={() => {
								diconnectWallet();
							}}
						>
							Disconnect
						</li>
					</ul>
				) : null}
			</div>
		);
	} else {
		return (
			<div
				className="flex w-44 h-12 wallet-button items-center justify-center relative "
				onMouseEnter={() => updatehover(true)}
				onMouseLeave={() => updatehover(false)}
			>
				Connect Wallet
				{ishovered ? (
					<ul className="absolute w-full top-9 wallet-list">
						{availableWallets.map((wallet) => (
							<li
								className="flex justify-start w-full wallet-item"
								onClick={() => {
									connectWallet(wallet);
								}}
								key={wallet}
							>
								<a className="flex items-center">
									<img
										className="mx-1 "
										src={`/wallet-icons/${wallet.toLowerCase()}.svg`}
										height="30"
										width="30"
									></img>
									<p className="mx-2"> {wallet} </p>
								</a>
							</li>
						))}
					</ul>
				) : null}
			</div>
		);
	}

	/*return (
		<div
			className="wallet-button relative"
			onMouseEnter={() => updatehover(true)}
			onMouseLeave={() => updatehover(false)}
		>
			Connect Wallet
			{ishovered ? (
				<ul className="absolute w-full left-0 wallet-list">
					{availableWallets.map((wallet) => (
						<li
							className="flex justify-start w-full wallet-item"
							onClick={() => {
								connectWallet(wallet);
							}}
							key={wallet}
						>
							<a className="flex items-center">
								<img
									className="mx-1 "
									src={`/wallet-icons/${wallet.toLowerCase()}.svg`}
									height="30"
									width="30"
								></img>
								<p className="mx-2"> {wallet} </p>
							</a>
						</li>
					))}
				</ul>
			) : null}
		</div>
	);*/
};

export default WalletConnect;
