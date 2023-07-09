import Image from "next/image";
import { CardanoWallet, useWallet } from "@meshsdk/react";
import Router from "next/router";
import React from "react";
import { firestore_db } from "../../src/firebase-config";
import { getMatchs } from "../../src/dbcalls";
import { useState, useEffect } from "react";
import { DocumentData } from "firebase/firestore";
import { ArrowPathIcon } from "@heroicons/react/20/solid";
import { showDate } from "../../src/utils";

const BetComponent = (props: DocumentData) => {
	const { data } = props;
	return (
		<div className="flex flex-row w-full justify-center my-10">
			<div className="flex flex-col ">
				<h1 className="text-center font-extrabold tracking-tight text-gray-800 md:text-4xl lg:text-6xl text-white">
					{data.team_a} - {data.team_b}
				</h1>
				<h2 className="py-8 text-center font-bold tracking-tight text-gray-800 md:text-2xl lg:text-4xl text-white">
					{showDate(data.date)}
				</h2>
				<hr className="separator"></hr>
			</div>
		</div>
	);
};
export default BetComponent;
