import { Blockfrost, Lucid, Assets, Network } from "lucid-cardano";

const initLucid = async (wallet: string) => {
	let network: Network = process.env.NEXT_PUBLIC_CARDANOENV as Network;
	const newlucid = await Lucid.new(new Blockfrost("/api/blockfrost"), network);
	const api = await window.cardano[wallet.toLowerCase()].enable();
	newlucid.selectWallet(api);
	return newlucid;
};

function sumAssets(...assets: Assets[]) {
	return assets.reduce((a, b) => {
		for (const k in b) {
			if (b.hasOwnProperty(k)) {
				a[k] = (a[k] || 0n) + b[k];
			}
		}
		return a;
	}, {});
}

async function getBalance(lucid: Lucid, addr: string) {
	if (lucid) {
		const utxo_lovelace = await lucid.utxosAt(addr);
		const summedassets = utxo_lovelace
			.map((utxo) => utxo.assets)
			.reduce((acc, assets) => sumAssets(acc, assets), {});
		return summedassets.lovelace;
	}
}

export default initLucid;
export { getBalance };
