open Connection__Download
module Labels = Connection__UI__Labels

let toLabel = Labels.channelLabel

let detail = Labels.channelDetail

type pickerItem = {
  label: string,
  description: string,
  detail: string,
  value: string,
}

let pickerItem = (channel: Channel.t, ~selectedChannel: Channel.t): pickerItem => {
  let description = channel == selectedChannel ? "selected" : ""
  {
    label: toLabel(channel),
    description,
    detail: detail(channel),
    value: Channel.toString(channel),
  }
}

let pickerItems = (~selectedChannel: Channel.t): array<pickerItem> =>
  [Channel.LatestALS, Channel.DevALS]->Array.map(ch => pickerItem(ch, ~selectedChannel))
