function showDate(date: any) {
	return date.toDate().toLocaleString("en-US", {
		weekday: "short",
		day: "numeric",
		month: "short",
		hour: "numeric",
		hour12: false,
		minute: "2-digit",
	});
}

const shortAddress = (address?: string | null) =>
	address ? `${address.slice(0, 25)}...${address.slice(-4)}` : address;

function generateUniqueId() {
	return `${Date.now()}-${Math.random().toString().substr(2, 9)}`;
}

export { showDate, shortAddress, generateUniqueId };
