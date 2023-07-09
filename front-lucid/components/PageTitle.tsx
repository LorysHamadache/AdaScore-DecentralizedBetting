import Image from "next/image";
import { CardanoWallet, useWallet } from "@meshsdk/react";
import { useState } from "react";
import Router from "next/router";
import React from "react";

const PageTitle = (props: any) => {
	const title = props.Title;
	return (
		<div className="flex items-center mb-14">
			<h1 className="font-extrabold tracking-tight text-gray-800 md:text-4xl lg:text-6xl text-white">
				{title}
			</h1>
		</div>
	);
};
export default PageTitle;
