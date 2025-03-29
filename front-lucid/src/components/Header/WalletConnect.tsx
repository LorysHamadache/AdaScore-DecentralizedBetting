import React, { useState, useEffect, useContext, useRef } from "react";
import { Lucid, WalletApi } from "lucid-cardano";

import { ToastContext } from "@/components/Utils/Toast";
import Spinner from "@/components/Utils/Spinner";

import initLucid, { getBalance } from "@/functions/chain/lucid";
import { useStoreActions, useStoreState } from "@/functions/wallet-store";
import { shortAddress, generateUniqueId } from "@/functions/utils";

const WalletConnect = () => {
	//Wallet
	const [ishovered, updatehover] = useState(false);
	const [loading, setloading] = useState(true);

	//Lucid
	const [lucid, setLucid] = useState<Lucid | null>(null);
	const [api, setApi] = useState<WalletApi | null>(null);
	const [balance, setBalance] = useState<string>("");

	// WalletStore Init
	const walletStore = useStoreState((state) => state.wallet);
	const setWallet = useStoreActions((actions) => actions.setWallet);
	const availableWallets = useStoreState((state) => state.availableWallets);
	const setAvailableWallets = useStoreActions(
		(actions) => actions.setAvailableWallets
	);

	const autoconnect_effectRan = useRef(false);
	// Toaster
	const { addToast } = useContext(ToastContext)!;

	function createToaster(mes: string, type: string) {
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
		if (autoconnect_effectRan.current === false) {
			loadWalletSession();
			autoconnect_effectRan.current = true;
		}
	}, []);

	const connectWallet = async (wallet: string) => {
		setloading(true);
		let newapi: WalletApi | null = await window.cardano[
			wallet.toLowerCase()
		].enable();
		let newlucid: Lucid | null = null;
		let newwalletStoreObj = walletStore;
		let newbalance: string = "Error";

		if (window.cardano && newapi) {
			if (
				(await newapi.getNetworkId()).toString() !=
				process.env.NEXT_PUBLIC_CARDANOENVNB
			) {
				newapi = null;
				newwalletStoreObj = { connected: false, name: "", address: "" };
				createToaster("Error: Wrong Network - Disconnected", "alert");
			} else {
				const newlucid = await initLucid(wallet);
				const addr = await newlucid.wallet.address();
				newwalletStoreObj = { connected: true, name: wallet, address: addr };
				const lvbalance: bigint = (await getBalance(newlucid, addr)) || 0n;
				newbalance = (lvbalance / 1000000n).toString();
				createToaster("Connected: " + shortAddress(addr), "info");
			}
		} else {
			newwalletStoreObj = { connected: false, name: "", address: "" };
			newapi = null;
			newwalletStoreObj = { connected: false, name: "", address: "" };
			createToaster("Disconnected", "alert");
		}
		updatehover(false);
		setApi(newapi);
		setloading(false);
		setWallet(newwalletStoreObj);
		setBalance(newbalance);
		setLucid(newlucid);
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
};

export default WalletConnect;
