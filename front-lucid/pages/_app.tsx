import "@/styles/globals.css";
import type { AppProps } from "next/app";
import { StoreProvider } from "easy-peasy";
import { MeshProvider } from "@meshsdk/react";
import walletStore from "../src/store";
import {
	ToastProvider,
	ToastContainer,
} from "../components/UtilsComponents/Toast";

export default function App({ Component, pageProps }: AppProps) {
	return (
		<MeshProvider>
			<ToastProvider>
				<StoreProvider store={walletStore}>
					<Component {...pageProps} />
				</StoreProvider>
				<ToastContainer />
			</ToastProvider>
		</MeshProvider>
	);
}
