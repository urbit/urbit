import React, { Component } from 'react';
import { Sigil } from './icons/sigil';

import { api } from '/api';
import { Route, Link } from 'react-router-dom';
import { EditElement } from '/components/lib/edit-element';
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
      notesToSet: null
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
    // sigil color updates are done by keystroke parsing on update
    // other field edits are exclusively handled by setField()
    let currentColor = (props.contact.color) ? props.contact.color : "000000";
    currentColor = uxToHex(currentColor);
    let hexExp = /([0-9A-Fa-f]{6})/
    let hexTest = hexExp.exec(this.state.colorToSet);

    if (hexTest && (hexTest[1] !== currentColor) && !props.share) {
      api.contactEdit(props.path, `~${props.ship}`, {color: hexTest[1]});
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
      case "email": {
        if (
          (state.emailToSet === "") ||
          (state.emailToSet === props.contact.email)
        ) {
          return false;
        }
        let emailTestResult = emailTest.exec(state.emailToSet);
        if (emailTestResult) {
          api.contactEdit(props.path, ship, { email: state.emailToSet });
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
        api.contactEdit(props.path, ship, { nickname: state.nickNameToSet });
        break;
      }
      case "notes": {
        if (
          (state.notesToSet === "") ||
          (state.notesToSet === props.contact.notes)
        ) {
          return false;
        }
        api.contactEdit(props.path, ship, { notes: state.notesToSet });
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
          api.contactEdit(props.path, ship, { phone: state.phoneToSet });
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
          api.contactEdit(props.path, ship, { website: state.websiteToSet });
        }
        break;
      }
      case "removeAvatar": {
        api.contactEdit(props.path, ship, { avatar: null });
        break;
      }
      case "removeEmail": {
        this.setState({ emailToSet: "" });
        api.contactEdit(props.path, ship, { email: "" });
        break;
      }
      case "removeNickname": {
        this.setState({ nicknameToSet: "" });
        api.contactEdit(props.path, ship, { nickname: "" });
        break;
      }
      case "removePhone": {
        this.setState({ phoneToSet: "" });
        api.contactEdit(props.path, ship, { phone: "" });
        break;
      }
      case "removeWebsite": {
        this.setState({ websiteToSet: "" });
        api.contactEdit(props.path, ship, { website: "" });
        break;
      }
      case "removeNotes": {
        this.setState({ notesToSet: "" });
        api.contactEdit(props.path, ship, { notes: "" });
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

    api.contactView.share(
      `~${props.ship}`, props.path, `~${window.ship}`, contact
    );
    this.editToggle();
  }

  removeFromGroup() {
    const { props } = this;
    api.contactHook.remove(props.path, `~${props.ship}`);
    props.history.push(`/~contacts${props.path}`);
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
            className="b--gray4 black f7 ba db pl2"
            onChange={this.sigilColorSet}
            defaultValue={defaultColor}
            key={"default" + defaultColor}
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
          color={currentColor}
          key={"avatar" + currentColor} />;

    return (
      <div className="w-100 mt8 flex justify-center pa4 pt8 pt0-l pa0-xl pt4-xl">
        <div className="w-100 mw6 tc">
          {avatar}
          {sigilColor}
          <button className="f9 b--black ba pa2">Upload an Image</button>
          {removeImage}
          <div className="w-100 pt8 lh-copy tl">
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
        color={hexColor}
        key={hexColor} />;

    let websiteHref =
      (props.contact.website && props.contact.website.includes("://")) ?
      props.contact.website : "http://" + props.contact.website;

    return (
      <div className="w-100 mt8 flex justify-center pa4 pt8 pt0-l pa0-xl pt4-xl">
        <div className="w-100 mw6 tc">
          {avatar}
          <div className="w-100 pt8 lh-copy tl">
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
      state.edit ? "Finish Editing" : "Edit Contact Info";
    if (props.share && state.edit) {
      editInfoText = "Share with Group";
    }

    let ourOpt = (props.ship === window.ship) ? "dib" : "dn";
    let localOpt =
      ((props.ship === window.ship) && (props.path === "/~/default"))
      ? "dib" : "dn";
    let adminOpt =
      (props.path.includes(window.ship) || (props.ship === window.ship))
      ? "dib" : "dn";

    let card = state.edit ? this.renderEditCard() : this.renderCard();

    //TODO "Share card" if it's /me -> sends to /~/default of recipient
    return (
      <div className="h-100 w-100 overflow-x-hidden">
        <div className="w-100 bg-white fixed bb b--gray4">
          <div className="w-100 h2 dn-m dn-l dn-xl inter pb6 pl3 pt3 f8">
            <Link to="/~contacts/">{"⟵"}</Link>
          </div>
          <button 
            onClick={() => {
              if (props.share) {
                this.shareWithGroup();
              } else {
                this.editToggle();
              }
            }}
            className={`ml3 mt2 mb2 f9 pa1 ba br2 pointer b--black ` + ourOpt}>
            {editInfoText}
          </button>
          <button
            className={`ml3 mt2 mb2 f9 pa1 ba br2 b--black ` + localOpt}>
            Share Contact Info
          </button>
          <button
            className={`ml3 mt2 mb2 f9 pa1 ba red2 br2 b--red2 pointer ` + adminOpt}
            onClick={this.removeFromGroup}>
            Remove from Group
          </button>
        </div>
        {card}
      </div>
    );
  }
}

