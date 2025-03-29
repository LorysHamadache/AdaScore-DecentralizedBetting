import Image from "next/image";
import React from "react";

const FrontGallery = () => {
	return (
		<div className="flex items-center mb-14">
			<div className="basis-4/6 px-3 py-3">
				<h1 className="font-extrabold tracking-tight md:text-4xl lg:text-6xl text-white">
					The First Cardano Decentralized Betting Platform
				</h1>
			</div>
			<div className="basis-1/6 card py-12 px-4 mx-14">
				<Image
					src="/frontgallery/championsleague.png"
					alt="Logo"
					className="logo"
					width={250}
					height={100}
					priority
				></Image>
			</div>
		</div>
	);
};

export default FrontGallery;
