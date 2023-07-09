import { scriptAddress } from "./scriptconfig";

const blockfrostProvider = new BlockfrostProvider(
	"preprodzA1EHBzstKZ9JNcKOUKK1lKAhkv9NXdo"
);

async function fetchScriptUtxo(txhash: string) {
	const utxos = await blockfrostProvider.fetchAddressUTxOs(scriptAddress);
	let response = utxos.find((utxo: any) => {
		return utxo.input.txHash == txhash;
	});
	return response;
}

async function fetchRefScriptUtxo() {
	const utxos = await blockfrostProvider.fetchAddressUTxOs(scriptAddress);
	let response = utxos.find((utxo: any) => {
		return (
			utxo.input.txHash ==
			"960b31818a81e86c3d6d29fd4ea5a51739f6b61bc099f9e616856ccae1ce849e"
		);
	});
	console.log(utxos);
	return response;
}

async function fetchRefScriptUtxo2() {
	const utxos = await blockfrostProvider.fetchAddressUTxOs(
		"addr_test1qp298ux4axxgsud5ruu7tj2mhuc2gwv8ylm8y7r4ja9ezy9lpdrvx4x8x5ns6gdwlsxpvrg53yeydc08a3qaq2uwatjqqfnn8w"
	);
	let response = utxos.find((utxo: any) => {
		return (
			utxo.input.txHash ==
			"8ecbfc388a04339492c8968f624977ef1081ca85d8b463347dd5b199a210b235"
		);
	});
	console.log(utxos);
	return response;
}

export {
	blockfrostProvider,
	fetchScriptUtxo,
	fetchRefScriptUtxo,
	fetchRefScriptUtxo2,
};
