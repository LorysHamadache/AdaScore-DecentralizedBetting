import Image from "next/image";
import {
	Cog6ToothIcon,
	ChatBubbleBottomCenterTextIcon,
	AcademicCapIcon,
} from "@heroicons/react/24/solid";
import WalletConnect from "@/components/Header/WalletConnect";

const Header = () => {
	return (
		<header className="head flex justify-between items-center md:p-4">
			<Image
				src="/Logo.png"
				alt="Logo"
				className="logo"
				width={250}
				height={100}
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
