module Labels = Connection__UI__Labels

let toLabel = Labels.channelLabel

let detail = Labels.channelDetail

type pickerItem = {
  label: string,
  description: string,
  detail: string,
  value: string,
}

let pickerItem = (
  channel: Connection__Download__Channel.t,
  ~selectedChannel: Connection__Download__Channel.t,
): pickerItem => {
  let description = channel == selectedChannel ? "selected" : ""
  {
    label: toLabel(channel),
    description,
    detail: detail(channel),
    value: Connection__Download__Channel.toString(channel),
  }
}

let pickerItems = (~selectedChannel: Connection__Download__Channel.t): array<pickerItem> =>
  [Connection__Download__Channel.LatestALS, Connection__Download__Channel.DevALS]
  ->Array.map(ch => pickerItem(ch, ~selectedChannel))
