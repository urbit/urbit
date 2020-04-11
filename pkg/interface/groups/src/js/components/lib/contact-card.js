import React, { Component } from 'react';
import { Sigil } from './icons/sigil';

import { api } from '/api';
import { Route, Link } from 'react-router-dom';
import { EditElement } from '/components/lib/edit-element';
import { Spinner } from './icons/icon-spinner';
import { uxToHex } from '/lib/util';

export class ContactCard extends Component {
  constructor(props) {
    super(props);
    this.state = {
      edit: props.share,
      colorToSet: null,
      nickNameToSet: null,
      emailToSet: null,
      phoneToSet: null,
      websiteToSet: null,
      notesToSet: null,
      awaiting: false,
      type: "Saving to group"
    };
    this.editToggle = this.editToggle.bind(this);
    this.sigilColorSet = this.sigilColorSet.bind(this);
    this.nickNameToSet = this.nickNameToSet.bind(this);
    this.emailToSet = this.emailToSet.bind(this);
    this.phoneToSet = this.phoneToSet.bind(this);
    this.websiteToSet = this.websiteToSet.bind(this);
    this.notesToSet = this.notesToSet.bind(this);
    this.setField = this.setField.bind(this);
    this.shareWithGroup = this.shareWithGroup.bind(this);
    this.removeFromGroup = this.removeFromGroup.bind(this);
  }

  componentDidUpdate(prevProps) {
    const { props } = this;
    if (props.ship !== prevProps.ship) {
      this.setState({
        edit: props.share,
        colorToSet: null,
        nickNameToSet: null,
        emailToSet: null,
        phoneToSet: null,
        websiteToSet: null,
        notesToSet: null
      });
      return;
    }
  }

  editToggle() {
    const { props } = this;
    let editSwitch = this.state.edit;
    editSwitch = !editSwitch;
    this.setState({edit: editSwitch});
  }

  emailToSet(value) {
    this.setState({ emailToSet: value });
  }

  nickNameToSet(value) {
    this.setState({ nickNameToSet: value });
  }

  notesToSet(value) {
    this.setState({ notesToSet: value });
  }

  phoneToSet(value) {
    this.setState({ phoneToSet: value });
  }

  websiteToSet(value) {
    this.setState({ websiteToSet: value });
  }

  sigilColorSet(event) {
    this.setState({ colorToSet: event.target.value });
  }

  shipParser(ship) {
    switch (ship.length) {
      case 3: return "Galaxy";
      case 6: return "Star";
      case 13: return "Planet";
      case 56: return "Comet";
      default: return "Unknown";
    }
  }

  setField(field) {
    const { props, state } = this;
    let ship = "~" + props.ship;
    let emailTest = new RegExp(''
      + /[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*/.source
      + /@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?/.source
    );

    let phoneTest = new RegExp(''
      + /^\s*(?:\+?(\d{1,3}))?/.source
      + /([-. (]*(\d{3})[-. )]*)?((\d{3})[-. ]*(\d{2,4})(?:[-.x ]*(\d+))?)\s*$/.source
    );

    let websiteTest = new RegExp(''
      + /[(http(s)?):\/\/(www\.)?a-zA-Z0-9@:%._\+~#=]{2,256}/.source
      + /\.[a-z]{2,6}\b([-a-zA-Z0-9@:%_\+.~#?&//=]*)/.source
    );

    switch (field) {
      case "color": {
        let currentColor = (props.contact.color) ? props.contact.color : "000000";
        currentColor = uxToHex(currentColor);
        let hexExp = /([0-9A-Fa-f]{6})/
        let hexTest = hexExp.exec(this.state.colorToSet);

        if (hexTest && (hexTest[1] !== currentColor) && !props.share) {
          this.setState({ awaiting: true, type: "Saving to group" }, (() => {
            api.contactEdit(props.path, `~${props.ship}`, { color: hexTest[1] }).then(() => {
              this.setState({ awaiting: false });
            });
          }))
        }
        break;
      }
      case "email": {
        if (
          (state.emailToSet === "") ||
          (state.emailToSet === props.contact.email)
        ) {
          return false;
        }
        let emailTestResult = emailTest.exec(state.emailToSet);
        if (emailTestResult) {
          this.setState({ awaiting: true, type: "Saving to group" }, (() => {
            api.contactEdit(props.path, ship, { email: state.emailToSet }).then(() => {
              this.setState({awaiting: false});
            });
          }))
        }
        break;
      }
      case "nickname": {
        if (
          (state.nickNameToSet === "") ||
          (state.nickNameToSet === props.contact.nickname)
        ) {
          return false;
        }
        this.setState({ awaiting: true, type: "Saving to group" }, (() => {
          api.contactEdit(props.path, ship, { nickname: state.nickNameToSet }).then(() => {
            this.setState({ awaiting: false });
          });
        }))

        break;
      }
      case "notes": {
        if (
          (state.notesToSet === "") ||
          (state.notesToSet === props.contact.notes)
        ) {
          return false;
        }
        this.setState({ awaiting: true, type: "Saving to group" }, (() => {
          api.contactEdit(props.path, ship, { notes: state.notesToSet }).then(() => {
            this.setState({ awaiting: false });
          });
        }))
        break;
      }
      case "phone": {
        if (
          (state.phoneToSet === "") ||
          (state.phoneToSet === props.contact.phone)
        ) {
          return false;
        }
        let phoneTestResult = phoneTest.exec(state.phoneToSet);
        if (phoneTestResult) {
          this.setState({ awaiting: true, type: "Saving to group" }, (() => {
            api.contactEdit(props.path, ship, { phone: state.phoneToSet }).then(() => {
              this.setState({ awaiting: false });
            });
          }))
        }
        break;
      }
      case "website": {
        if (
          (state.websiteToSet === "") ||
          (state.websiteToSet === props.contact.website)
        ) {
          return false;
        }
        let websiteTestResult = websiteTest.exec(state.websiteToSet);
        if (websiteTestResult) {
          this.setState({ awaiting: true, type: "Saving to group" }, (() => {
            api.contactEdit(props.path, ship, { website: state.websiteToSet }).then(() => {
              this.setState({ awaiting: false });
            });
          }))
        }
        break;
      }
      case "removeAvatar": {
        this.setState({ awaiting: true, type: "Removing from group" }, (() => {
            api.contactEdit(props.path, ship, { avatar: null }).then(() => {
              this.setState({ awaiting: false });
            });
        }))
        break;
      }
      case "removeEmail": {
        this.setState({ emailToSet: "", awaiting: true, type: "Removing from group" }, (() => {
          api.contactEdit(props.path, ship, { email: "" }).then(() => {
            this.setState({awaiting: false});
          });
        }));
        break;
      }
      case "removeNickname": {
        this.setState({ nicknameToSet: "", awaiting: true, type: "Removing from group" }, (() => {
          api.contactEdit(props.path, ship, { nickname: "" }).then(() => {
            this.setState({awaiting: false});
          });
        }));
        break;
      }
      case "removePhone": {
        this.setState({ phoneToSet: "", awaiting: true, type: "Removing from group" }, (() => {
          api.contactEdit(props.path, ship, { phone: "" }).then(() => {
            this.setState({awaiting: false});
          });
        }));
        break;
      }
      case "removeWebsite": {
        this.setState({ websiteToSet: "", awaiting: true, type: "Removing from group" }, (() => {
          api.contactEdit(props.path, ship, { website: "" }).then(() => {
            this.setState({awaiting: false});
          });
        }));
        break;
      }
      case "removeNotes": {
        this.setState({ notesToSet: "", awaiting: true, type: "Removing from group" }, (() => {
          api.contactEdit(props.path, ship, { notes: "" }).then(() => {
            this.setState({awaiting: false});
          });
        }));
        break;
      }
    }
  }

  pickFunction(val, def) {
    if (val !== null) {
      return val;
    }
    return def;
  }

  shareWithGroup() {
    const { props, state } = this;
    let defaultVal = props.share ? {
      nickname: props.rootIdentity.nickname,
      email: props.rootIdentity.email,
      phone: props.rootIdentity.phone,
      website: props.rootIdentity.website,
      notes: props.rootIdentity.notes,
      color: uxToHex(props.rootIdentity.color)
    } : {
      nickname: props.contact.nickname,
      email: props.contact.email,
      phone: props.contact.phone,
      website: props.contact.website,
      notes: props.contact.notes,
      color: props.contact.color
    };

    let contact = {
      nickname: this.pickFunction(state.nickNameToSet, defaultVal.nickname),
      email: this.pickFunction(state.emailToSet, defaultVal.email),
      phone: this.pickFunction(state.phoneToSet, defaultVal.phone),
      website: this.pickFunction(state.websiteToSet, defaultVal.website),
      notes: this.pickFunction(state.notesToSet, defaultVal.notes),
      color: this.pickFunction(state.colorToSet, defaultVal.color),
      avatar: null
    };
    this.setState({awaiting: true, type: "Sharing with group"}, (() => {
      api.contactView.share(
        `~${props.ship}`, props.path, `~${window.ship}`, contact
      ).then(() => {
        props.history.push(`/~groups/view${props.path}/${window.ship}`)
      });
    }))
  }

  removeFromGroup() {
    const { props } = this;
    // share empty contact so that we can remove ourselves from group
    // if we haven't shared yet
    let contact = {
      nickname: "",
      email: "",
      phone: "",
      website: "",
      notes: "",
      color: "000000",
      avatar: null
    };

    api.contactView.share(
      `~${props.ship}`, props.path, `~${window.ship}`, contact
    );

    this.setState({awaiting: true, type: "Removing from group"}, (() => {
      api.contactHook.remove(props.path, `~${props.ship}`).then(() => {
        let destination = (props.ship === window.ship)
          ? "" : props.path;
        this.setState({awaiting: false});
        props.history.push(`/~groups${destination}`);
      });
    }))
  }

  renderEditCard() {
    const { props, state } = this;
    // if this is our first edit in a new group, propagate from root identity
    let defaultValue = props.share ? {
      nickname: props.rootIdentity.nickname,
      email: props.rootIdentity.email,
      phone: props.rootIdentity.phone,
      website: props.rootIdentity.website,
      notes: props.rootIdentity.notes,
      color: props.rootIdentity.color
    } : {
      nickname: props.contact.nickname,
      email: props.contact.email,
      phone: props.contact.phone,
      website: props.contact.website,
      notes: props.contact.notes,
      color: props.contact.color
    };

    let shipType = this.shipParser(props.ship);

    let defaultColor = !!defaultValue.color ? defaultValue.color : "000000";
    defaultColor = uxToHex(defaultColor);
    let currentColor = !!state.colorToSet ? state.colorToSet : defaultColor;
    currentColor = uxToHex(currentColor);

    let sigilColor = "";
    let hasAvatar =
      'avatar' in props.contact && props.contact.avatar !== "TODO";

    if (!hasAvatar) {
      sigilColor = (
        <div className="tl mt4 mb4 w-auto ml-auto mr-auto"
          style={{ width: "fit-content" }}>
          <p className="f9 gray2 lh-copy">Sigil Color</p>
          <textarea
            className={"b--gray4 b--gray2-d black white-d bg-gray0-d f7 ba db pl2 " +
            "focus-b--black focus-b--white-d"}
            onChange={this.sigilColorSet}
            defaultValue={defaultColor}
            key={"default" + defaultColor}
            onBlur={(() => this.setField("color"))}
            style={{
              resize: "none",
              height: 40,
              paddingTop: 10,
              width: 114
            }}>
          </textarea>
        </div>
      );
    }

    let removeImage = hasAvatar ? (
        <div>
          <button className="f9 black pointer db"
            onClick={() => this.setField("removeAvatar")}>
            Remove photo
          </button>
        </div>
      ) : "";

    let avatar = (hasAvatar)
      ? <img className="dib h-auto" width={128} src={props.contact.avatar} />
      : <Sigil
          ship={props.ship}
          size={128}
          color={"#" + currentColor}
          key={"avatar" + currentColor} />;

    return (
      <div className="w-100 mt8 flex justify-center pa4 pt8 pt0-l pa0-xl pt4-xl pb8">
        <div className="w-100 mw6 tc">
          {avatar}
          {sigilColor}
          {removeImage}
          <div className="w-100 pt8 pb8 lh-copy tl">
            <p className="f9 gray2">Ship Name</p>
            <p className="f8 mono">~{props.ship}</p>
            <p className="f9 gray2 mt3">Ship Type</p>
            <p className="f8">{shipType}</p>
            <hr className="mv8 gray4 b--gray4 bb-0 b--solid" />
            <EditElement
              title="Nickname"
              defaultValue={defaultValue.nickname}
              onChange={this.nickNameToSet}
              onDeleteClick={() => this.setField("removeNickname")}
              onSaveClick={() => this.setField("nickname")}
              showButtons={!props.share} />
            <EditElement
              title="Email"
              defaultValue={defaultValue.email}
              onChange={this.emailToSet}
              onDeleteClick={() => this.setField("removeEmail")}
              onSaveClick={() => this.setField("email")}
              showButtons={!props.share} />
            <EditElement
              title="Phone"
              defaultValue={defaultValue.phone}
              onChange={this.phoneToSet}
              onDeleteClick={() => this.setField("removePhone")}
              onSaveClick={() => this.setField("phone")}
              showButtons={!props.share} />
            <EditElement
              title="Website"
              defaultValue={defaultValue.website}
              onChange={this.websiteToSet}
              onDeleteClick={() => this.setField("removeWebsite")}
              onSaveClick={() => this.setField("website")}
              showButtons={!props.share} />
            <EditElement
              title="Notes"
              defaultValue={defaultValue.notes}
              onChange={this.notesToSet}
              onDeleteClick={() => this.setField("removeNotes")}
              onSaveClick={() => this.setField("notes")}
              resizable={true}
              showButtons={!props.share} />
          </div>
        </div>
      </div>
    );
  }

  renderCard() {
    const { props } = this;
    let shipType = this.shipParser(props.ship);
    let currentColor = props.contact.color ? props.contact.color : "0x0";
    let hexColor = uxToHex(currentColor);

    let avatar =
      ('avatar' in props.contact && props.contact.avatar !== "TODO") ?
      <img className="dib h-auto" width={128} src={props.contact.avatar} /> :
      <Sigil
        ship={props.ship}
        size={128}
        color={"#" + hexColor}
        key={hexColor} />;

    let websiteHref =
      (props.contact.website && props.contact.website.includes("://")) ?
      props.contact.website : "http://" + props.contact.website;

    return (
      <div className="w-100 mt8 flex justify-center pa4 pt8 pt0-l pa0-xl pt4-xl">
        <div className="w-100 mw6 tc">
          {avatar}
          <div className="w-100 pv8 lh-copy tl">
            <p className="f9 gray2">Ship Name</p>
            <p className="f8 mono">~{props.ship}</p>
            <p className="f9 gray2 mt3">Ship Type</p>
            <p className="f8">{shipType}</p>
            <hr className="mv8 gray4 b--gray4 bb-0 b--solid" />
            <div>
              { !!props.contact.nickname ? (
                  <div>
                    <p className="f9 gray2">Nickname</p>
                    <p className="f8">{props.contact.nickname}</p>
                  </div>
                ) : null
              }
              { !!props.contact.email ? (
                  <div>
                    <p className="f9 mt6 gray2">Email</p>
                    <p className="f8">{props.contact.email}</p>
                  </div>
                ) : null
              }
              { !!props.contact.phone ? (
                  <div>
                    <p className="f9 mt6 gray2">Phone</p>
                    <p className="f8">{props.contact.phone}</p>
                  </div>
                ) : null
              }
              { !!props.contact.website ? (
                  <div>
                    <p className="f9 mt6 gray2">website</p>
                    <a target="_blank"
                       className="bb b--black f8"
                       href={websiteHref}>
                      {props.contact.website}
                    </a>
                  </div>
                ) : null
              }
              { !!props.contact.notes ? (
                  <div>
                    <p className="f9 mt6 gray2">notes</p>
                    <p className="f8">{props.contact.notes}</p>
                  </div>
                ) : null
              }
            </div>
          </div>
        </div>
      </div>
    );
  }

  render() {
    const { props, state } = this;

    let editInfoText =
      state.edit ? "Finish" : "Edit";
    if (props.share && state.edit) {
      editInfoText = "Share";
    }

    let ourOpt = (props.ship === window.ship) ? "dib" : "dn";

    let adminOpt =
       ((props.path.includes(`~${window.ship}/`)) || ((props.ship === window.ship) &&
        !(props.path.includes('/~/default'))))
       ? "dib" : "dn";

    let meLink = (props.path === "/~/default")
      ? `/~groups` : `/~groups/detail${props.path}`;

    let card = state.edit ? this.renderEditCard() : this.renderCard();
    return (
      <div className="w-100 h-100 overflow-hidden">
        <div
          className={
            "flex justify-between w-100 bg-white bg-gray0-d " +
            "bb b--gray4 b--gray1-d "
          }>
          <div className="f9 mv4 mh3 pt1 dib w-100">
            <Link to={meLink}>
              {"⟵"}
            </Link>
          </div>
          <div className="flex">
            <button
              onClick={() => {
                if (props.share) {
                  this.shareWithGroup();
                } else {
                  this.editToggle();
                }
              }}
              className={
                `white-d bg-gray0-d mv4 mh3 f9 pa1 pointer flex-shrink-0 ` +
                ourOpt
              }>
              {editInfoText}
            </button>
          </div>
          <button
            className={
              `bg-gray0-d mv4 mh3 pa1 f9 red2 pointer flex-shrink-0 ` + adminOpt
            }
            onClick={this.removeFromGroup}>
            {props.ship === window.ship
              ? "Leave Group"
              : "Remove from Group"}
          </button>
        </div>
        <div className="h-100 w-100 overflow-x-hidden pb8 white-d">{card}</div>
        <Spinner awaiting={this.state.awaiting} text={`${this.state.type}...`} classes="absolute right-1 bottom-1 ba pa2 b--gray1-d" />
      </div>
    );
  }
}
