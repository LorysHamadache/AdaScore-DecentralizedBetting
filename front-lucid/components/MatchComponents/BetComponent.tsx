import Image from "next/image";
import Router from "next/router";
import React from "react";
import { firestore_db } from "../../src/firebase-config";
import { getMatchs } from "../../src/dbcalls";
import { useState, useEffect } from "react";
import { DocumentData } from "firebase/firestore";
import { ArrowPathIcon } from "@heroicons/react/20/solid";

const BetComponent = () => {
	const [data, setData] = useState<DocumentData[]>([]);
	const [bet_filter, setFilter] = useState("");
	const [refreshCount, setRefreshCount] = useState(0);

	return (
		<div className="w-full flex flex-col justify-center my-10">
			<table className="table-match basis-7/12 border-collapse">
				<tbody>
					<tr className="row-match flex w-full items-center">
						<td className="text-center ">
							No Bet for this match found, you can create your own above
						</td>
					</tr>
				</tbody>
			</table>
		</div>
	);
};
export default BetComponent;
