import "@/styles/globals.css";
import type { AppProps } from "next/app";
import { StoreProvider } from "easy-peasy";
import walletStore from "@/functions/wallet-store";
import { ToastProvider, ToastContainer } from "@/components/Utils/Toast";

export default function App({ Component, pageProps }: AppProps) {
	return (
		<ToastProvider>
			<StoreProvider store={walletStore}>
				<Component {...pageProps} />
			</StoreProvider>
			<ToastContainer />
		</ToastProvider>
	);
}
