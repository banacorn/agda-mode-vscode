open Connection__Download

let toLabel = (channel: Channel.t): string =>
  switch channel {
  | Channel.LatestALS => "Latest"
  | Channel.DevALS => "Development"
  }

let detail = (channel: Channel.t): string =>
  switch channel {
  | Channel.LatestALS => "Tracks the latest stable release"
  | Channel.DevALS => "Tracks the latest commit of the master branch"
  }

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
