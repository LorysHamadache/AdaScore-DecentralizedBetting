import { createTypedHooks } from "easy-peasy";
import { Action, action } from "easy-peasy";
import { createStore, persist } from "easy-peasy";

// WalletStore
interface WalletStoreInterface {
	connected: boolean;
	name: string;
	address: string;
}

interface StoreModelInterface {
	wallet: WalletStoreInterface;
	setWallet: Action<StoreModelInterface, WalletStoreInterface>;
	availableWallets: string[];
	setAvailableWallets: Action<StoreModelInterface, string[]>;
}

const storeModel: StoreModelInterface = {
	wallet: { connected: false, name: "", address: "" },
	setWallet: action((state, newWallet) => {
		state.wallet = newWallet;
	}),
	availableWallets: [],
	setAvailableWallets: action((state, newAvailableWallets) => {
		state.availableWallets = newAvailableWallets;
	}),
};

const store = createStore(persist(storeModel));
export default store;

/* Typed Hooks */
const { useStoreActions, useStoreState, useStoreDispatch, useStore } =
	createTypedHooks<StoreModelInterface>();

export { useStoreActions, useStoreState, useStoreDispatch, useStore };
