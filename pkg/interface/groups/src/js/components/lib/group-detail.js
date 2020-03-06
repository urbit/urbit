import React, { Component } from 'react';
import { Route, Link } from 'react-router-dom';
import { deSig, uxToHex } from '/lib/util.js';

export class GroupDetail extends Component {
  constructor(props) {
    super(props);
    this.state = {
      title: "",
      description: "",
    }
    this.changeTitle = this.changeTitle.bind(this);
    this.changeDescription = this.changeDescription.bind(this);
  }

  componentDidMount() {
    const { props } = this;
    let channelPath = `${props.path}/contacts${props.path}`;
    if ((props.association) && (props.association[channelPath])) {
      this.setState({
        title: props.association[channelPath].metadata.title,
        description: props.association[channelPath].metadata.description
      })
    }
  }

  componentDidUpdate(prevProps) {
    const { props } = this;
    if (prevProps !== this.props) {
      let channelPath = `${props.path}/contacts${props.path}`;
      if ((props.association) && (props.association[channelPath])) {
        this.setState({
          title: props.association[channelPath].metadata.title,
          description: props.association[channelPath].metadata.description
        })
      }
    }
  }

  changeTitle(event) {
    this.setState({title: event.target.value})
  }

  changeDescription(event) {
    this.setState({description: event.target.value})
  }

  renderDetail() {
    const { props } = this;

    let responsiveClass =
      props.activeDrawer === "detail" ? "db " : "dn db-ns ";

    let isEmpty = (Object.keys(props.association).length === 0) ||
      ((Object.keys(props.association).length === 1) &&
        (Object.keys(props.association)[0].includes("contacts")));

    let channelList = (<div />);

    channelList = Object.keys(props.association).map((key) => {
      let channel = props.association[key];
      if (!('metadata' in channel)) {
        return <div key={channel} />;
      }

      if (channel["app-name"] === "contacts") {
        return <div key={channel} />;
      }

      let title = channel.metadata.title || channel["app-path"] || "";
      let color = uxToHex(channel.metadata.color) || "000000";
      let app = channel["app-name"] || "Unknown";
      let channelPath = channel["app-path"];
      let link = `/~${app}/join${channelPath}`
      app = app.charAt(0).toUpperCase() + app.slice(1)

      return (
        <li key={channelPath} className="f9 list flex pv2 w-100">
          <div className="dib"
            style={{ backgroundColor: `#${color}`, height: 32, width: 32 }}
          ></div>
          <div className="flex flex-column flex-auto">
            <p className="f9 inter ml2 w-100">{title}</p>
            <p className="f9 inter ml2 w-100"
              style={{ marginTop: "0.35rem" }}>
              <span className="f9 di mr2 inter">{app}</span>
              <a className="f9 di green2" href={link}>Open</a>
            </p>
          </div>
        </li>
      )
    })

    let backLink = props.location.pathname;
    backLink = backLink.slice(0, props.location.pathname.indexOf("/detail"));

    let emptyGroup = (
      <div className={isEmpty ? "dt w-100 h-100" : "dn"}>
        <p className="gray2 f9 tc v-mid dtc">
          This group has no channels. To add a channel, invite this group using any application.
        </p>
      </div>
    );

    let title = props.path.substr(1);
    let description = "";
    let channel = `${props.path}/contacts${props.path}`;
    if ((props.association) && (props.association[channel])) {
      title = (props.association[channel].metadata.title !== "")
        ? props.association[channel].metadata.title
        : props.path.substr(1);
      description = (props.association[channel].metadata.description !== "")
        ? props.association[channel].metadata.description
        : "";
    }

    return (
      <div className={"relative h-100 w-100 bg-white bg-gray0-d white-d pa4 "
        + responsiveClass +
        ((isEmpty) ? "overflow-hidden" : "overflow-x-hidden")}>
        <div className="pb4 f8 db dn-m dn-l dn-xl">
          <Link to={backLink}>⟵ Contacts</Link>
        </div>
        <div className="w-100 lh-copy">
          <Link
            className="absolute right-1 f9"
            to={"/~groups/settings" + props.path}>Group Settings</Link>
          <p className="f9">{title}</p>
          <p className="f9 gray2">{description}</p>
          <p className="f9">
            {props.group.size + " participant" +
              ((props.group.size === 1) ? "" : "s")}
          </p>
        </div>
        <p className={"gray2 f9 mb2 pt6 " + (isEmpty ? "dn" : "")}>Group Channels</p>
        {emptyGroup}
        {channelList}
      </div>
    )
  }

  renderSettings() {
    const { props } = this;

    let groupOwner = (deSig(props.match.params.ship) === window.ship);

    let channelPath = `${props.path}/contacts${props.path}`;

    let association = ((props.association) && (props.association[channelPath]))
      ? props.association[channelPath] : {};

    return (
      <div className="pa4 w-100 h-100 white-d">
        <div className="f9 w-100">
          <Link to={"/~groups/detail" + props.path}>{"⟵ Channels"}</Link>
        </div>
        <div className={(groupOwner) ? "" : "o-30"}>
          <p className="f8 mt3 lh-copy">Rename</p>
          <p className="f9 gray2 mb4">Change the name of this group</p>
          <div className="relative w-100 flex"
          style={{maxWidth: "29rem"}}>
            <input
              className={"f8 ba b--gray3 b--gray2-d bg-gray0-d white-d " +
              "focus-b--black focus-b--white-d pa3 db w-100 flex-auto mr3"}
              value={this.state.title}
              disabled={!groupOwner}
              onChange={this.changeTitle}
            />
            <span className={"f8 absolute pa3 inter " + ((groupOwner) ? "pointer" : "")}
            style={{right: 12, top: 1}}
            ref="rename"
            onClick={() => {
              if (groupOwner) {
                props.api.setSpinner(true);
                props.api.metadataAdd(
                  association['app-path'],
                  association['group-path'],
                  this.state.title,
                  association.metadata.description,
                  association.metadata['date-created'],
                  uxToHex(association.metadata.color)
                ).then(() => {
                  this.refs.rename.innerText = "Saved";
                  props.api.setSpinner(false);
                })
              }
            }}>Save</span>
          </div>
          <p className="f8 mt3 lh-copy">Change description</p>
          <p className="f9 gray2 mb4">Change the description of this group</p>
          <div className="relative w-100 flex"
            style={{ maxWidth: "29rem" }}>
            <input
              className={"f8 ba b--gray3 b--gray2-d bg-gray0-d white-d " +
                "focus-b--black focus-b--white-d pa3 db w-100 flex-auto mr3"}
              value={this.state.description}
              disabled={!groupOwner}
              onChange={this.changeDescription}
            />
            <span className={"f8 absolute pa3 inter " + ((groupOwner) ? "pointer" : "")}
              style={{ right: 12, top: 1 }}
              ref="description"
              onClick={() => {
                if (groupOwner) {
                  props.api.setSpinner(true);
                  props.api.metadataAdd(
                    association['app-path'],
                    association['group-path'],
                    association.metadata.title,
                    this.state.description,
                    association.metadata['date-created'],
                    uxToHex(association.metadata.color)
                  ).then(() => {
                    this.refs.description.innerText = "Saved";
                    props.api.setSpinner(false);
                  })
                }
              }}>Save</span>
          </div>
        </div>
      </div>
    )
  }

  render() {
    let render = (this.props.settings)
      ? this.renderSettings() : this.renderDetail();

    return render;
  }
}

export default GroupDetail
