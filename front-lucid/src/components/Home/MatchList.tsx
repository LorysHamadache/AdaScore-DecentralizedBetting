import React, { useState, useEffect } from "react";
import { useRouter } from "next/router";

import { DocumentData } from "firebase/firestore";
import { firestore_db } from "@/functions/backend/firebase-init";
import { getMatchs } from "@/functions/backend/firestore-get";
import { showDate } from "@/functions/utils";

import { ArrowPathIcon } from "@heroicons/react/20/solid";

const MatchList = () => {
	const [data, setData] = useState<DocumentData[]>([]);
	const [match_filter, setFilter] = useState("");
	const [refreshCount, setRefreshCount] = useState(0);

	useEffect(() => {
		const fetchMatchs = async () => {
			const match_query = await getMatchs(firestore_db);
			setData(match_query.docs.map((doc) => ({ id: doc.id, ...doc.data() })));
		};
		fetchMatchs();
	}, [refreshCount]);

	const filter_result = data.filter((item) =>
		Object.values(item).some((field) =>
			field.toString().toLowerCase().includes(match_filter.toLowerCase())
		)
	);
	const filtered_data: DocumentData[] =
		match_filter == "" || filter_result.length == 0 ? data : filter_result;

	const handleRefresh = () => {
		setRefreshCount((prevCount) => prevCount + 1);
		setFilter("");
	};

	const router = useRouter();
	function handleClick(path: string) {
		router.push({ pathname: path });
	}

	return (
		<div className="w-full flex flex-col justify-center">
			<div className="flex my-2 items-center">
				<form className="basis-1/6">
					<input
						type="text"
						id="match_filter"
						name="match_filter"
						className="textbox-def h-9"
						value={match_filter}
						onChange={(e) => setFilter(e.target.value)}
						placeholder=" Team"
					/>
				</form>
				<div className="basis-5/6"></div>
				<button
					onClick={handleRefresh}
					className="h-9 nav-button flex items-center"
				>
					<ArrowPathIcon className="headericons "></ArrowPathIcon>
				</button>
			</div>
			<table className="table-match basis-7/12 border-collapse">
				<tbody>
					{filtered_data.map((match) => (
						<tr key={match.id} className="row-match flex w-full items-center">
							<td className="basis-4/6 md:text-xl lg:text-2xl font-semibold">
								{match.team_a} - {match.team_b}
							</td>
							<td className="basis-1/6 font-semibold md:text-lg lg:text-xl">
								{showDate(match.date)}
							</td>
							<td className="basis-1/6 text-right">
								<button
									className="nav-button font-semibold"
									onClick={() => handleClick("/match/" + match.id)}
								>
									Bet Now
								</button>
							</td>
						</tr>
					))}
				</tbody>
			</table>
		</div>
	);
};
export default MatchList;
