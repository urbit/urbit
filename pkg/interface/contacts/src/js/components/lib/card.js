import React, { Component } from 'react';
import { Sigil } from './icons/sigil';
import { uxToHex } from '/lib/util';

import { api } from '/api';
import { Route, Link } from 'react-router-dom';

export class ContactCard extends Component {
  constructor(props) {
    super(props);
    this.state = {
      edit: false,
      colorToSet: "",
      nickNameToSet: "",
      emailToSet: "",
      phoneToSet: "",
      websiteToSet: "",
      notesToSet: ""
    }
    this.editToggle = this.editToggle.bind(this);
    this.sigilColorSet = this.sigilColorSet.bind(this);
    this.nickNameToSet = this.nickNameToSet.bind(this);
    this.emailToSet = this.emailToSet.bind(this);
    this.phoneToSet = this.phoneToSet.bind(this);
    this.websiteToSet = this.websiteToSet.bind(this);
    this.notesToSet = this.notesToSet.bind(this);
    this.setField = this.setField.bind(this);
  }

  componentDidUpdate() {
    const { props } = this;
    // sigil color updates are done by keystroke parsing on update
    // other field edits are exclusively handled by setField()
    let currentColor = (props.contact.color) ? props.contact.color : "0x0";
    let currentHex = uxToHex(currentColor);
    let hexExp = /#?([0-9A-Fa-f]{6})/
    let hexTest = hexExp.exec(this.state.colorToSet);

    if ((hexTest) && (hexTest[1] !== currentHex)) {
      let ship = "~" + props.ship;
      api.contactEdit(props.path, ship, {color: hexTest[1]});
    }
  }

  editToggle() {
    let editSwitch = this.state.edit;
    editSwitch = !editSwitch;
    this.setState({edit: editSwitch});
  }

  emailToSet(event) {
    this.setState({ emailToSet: event.target.value });
  }

  nickNameToSet(event) {
    this.setState({ nickNameToSet: event.target.value });
  }

  notesToSet(event) {
    this.setState({ notesToSet: event.target.value });
  }

  phoneToSet(event) {
    this.setState({ phoneToSet: event.target.value });
  }

  sigilColorSet(event) {
    this.setState({ colorToSet: event.target.value });
  }

  websiteToSet(event) {
    this.setState({ websiteToSet: event.target.value });
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
        this.refs.email.value = "";
        break;
      }
      case "removeNickname": {
        this.setState({ nicknameToSet: "" });
        api.contactEdit(props.path, ship, { nickname: "" });
        this.refs.nickname.value = "";
        break;
      }
      case "removePhone": {
        this.setState({ phoneToSet: "" });
        api.contactEdit(props.path, ship, { phone: "" });
        this.refs.phone.value = "";
        break;
      }
      case "removeWebsite": {
        this.setState({ websiteToSet: "" });
        api.contactEdit(props.path, ship, { website: "" });
        this.refs.website.value = "";
        break;
      }
      case "removeNotes": {
        this.setState({ notesToSet: "" });
        api.contactEdit(props.path, ship, { notes: "" });
        this.refs.notes.value = "";
        break;
      }
    }
  }

  renderEditCard() {
    const { props } = this;
    // if this is our first edit in a new group, propagate from root identity
    let defaultValue = {
      nickname: (props.share) 
      ? props.rootIdentity.nickname
      : props.contact.nickname,
      email: (props.share)
      ? props.rootIdentity.email
      : props.contact.email,
      phone: (props.share)
      ? props.rootIdentity.phone
      : props.contact.phone,
      website: (props.share)
      ? props.rootIdentity.website
      : props.contact.website,
      notes: (props.share)
      ? props.rootIdentity.notes
      : props.contact.notes
    }

    let shipType = this.shipParser(props.ship);

    let currentColor = (props.contact.color) 
    ? props.contact.color 
    : "0x0";

    let hexColor = uxToHex(currentColor);

    let sigilColor = "";
    let hasAvatar = (props.contact.avatar !== "TODO");


    if (!hasAvatar) { 
      sigilColor = (
        <div className="tl mt4 mb4 w-auto ml-auto mr-auto"
        style={{ width: "fit-content" }}>
        <p className="f9 gray2 lh-copy">Sigil Color</p>
          <textarea
           className="b--gray4 black f7 ba db pl2"
           onChange={this.sigilColorSet}
           defaultValue={"#" + hexColor}
           style={{
             resize: "none",
             height: 40,
             paddingTop: 10,
              width: 114
            }}></textarea>
        </div>
      )
    }

    let removeImage = "";
    let avatar = (hasAvatar) 
      ? <img className="dib h-auto" width={128} src={props.contact.avatar} /> 
      : <Sigil ship={props.ship} size={128} color={"#" + hexColor} />;

    if (hasAvatar) {
      removeImage = (
        <div>
          <button class="f9 black pointer db"
            onClick={() => this.setField("removeAvatar")}>
            Remove photo
            </button>
        </div>
      )
    }

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

            <p className="f9 gray2">Nickname</p>
              <div className="w-100 flex">
                <textarea
                 ref="nickname"
                 className="w-100 ba pl3 b--gray4"
                 style={{ resize: "none",
                          height: 40,
                          paddingTop: 10 }}
                 onChange={this.nickNameToSet}
                defaultValue={defaultValue.nickname}/>
                <button
                  className={"f9 pointer ml3 ba pa2 pl3 pr3 b--red2 red2 " +
                    ((props.contact.nickname === "") ? "dn" : "dib")
                  }
                onClick={() => this.setField("removeNickname")}>
                Delete
                </button>
              </div>
              <button
                className={"pointer db mv2 f9 ba pa2 pl3 pr3 " +
                  ((
                    (props.contact.nickname === this.state.nickNameToSet)
                    || (this.state.nickNameToSet === "")
                  ) ? "b--gray4 gray4" : "b--black")
                }
                onClick={() => this.setField("nickname")}>
                Save
              </button>
            <p className="f9 gray2">Email</p>
            <div className="w-100 flex">
              <textarea
                ref="email"
                className="w-100 ba pl3 b--gray4"
                style={{
                  resize: "none",
                  height: 40,
                  paddingTop: 10
                }}
                onChange={this.emailToSet}
                defaultValue={defaultValue.email} />
              <button className={"f9 pointer ml3 ba pa2 pl3 pr3 b--red2 red2 " +
                ((props.contact.email === "") ? "dn" : "dib")}
                onClick={() => this.setField("removeEmail")}>
                Delete
              </button>
            </div>
            <button className={"pointer db mv2 f9 ba pa2 pl3 pr3 " +
              (((props.contact.email === this.state.emailToSet)
              || (this.state.emailToSet === ""))
                ? "b--gray4 gray4"
                : "b--black")}
              onClick={() => this.setField("email")}>
              Save
            </button>
            <p className="f9 gray2">Phone</p>
            <div className="w-100 flex">
              <textarea
                ref="phone"
                className="w-100 ba pl3 b--gray4"
                style={{
                  resize: "none",
                  height: 40,
                  paddingTop: 10
                }}
                onChange={this.phoneToSet}
                defaultValue={defaultValue.phone} />
              <button className={"f9 pointer ml3 ba pa2 pl3 pr3 b--red2 red2 " +
                ((props.contact.phone === "") ? "dn" : "dib")}
                onClick={() => this.setField("removePhone")}>
                Delete
                </button>
            </div>
            <button className={"pointer db mv2 f9 ba pa2 pl3 pr3 " +
              (((props.contact.phone === this.state.phoneToSet)
              || (this.state.phoneToSet === ""))
                ? "b--gray4 gray4"
                : "b--black")}
              onClick={() => this.setField("phone")}>
              Save
            </button>
            <p className="f9 gray2">Website</p>
            <div className="w-100 flex">
              <textarea
                ref="website"
                className="w-100 ba pl3 b--gray4"
                style={{
                  resize: "none",
                  height: 40,
                  paddingTop: 10
                }}
                onChange={this.websiteToSet}
                defaultValue={defaultValue.website} />
              <button className={"f9 pointer ml3 ba pa2 pl3 pr3 b--red2 red2 " +
                ((props.contact.website === "") ? "dn" : "dib")}
                onClick={() => this.setField("removeWebsite")}>
                Delete
                </button>
            </div>
            <button className={"pointer db mv2 f9 ba pa2 pl3 pr3 " +
              (((props.contact.website === this.state.websiteToSet)
                || (this.state.websitetoSet === ""))
                ? "b--gray4 gray4"
                : "b--black")}
              onClick={() => this.setField("website")}>
              Save
              </button>

            <p className="f9 gray2">Notes</p>
            <div className="w-100 flex">
              <textarea
                ref="notes"
                className="w-100 ba pl3 b--gray4"
                style={{
                  resize: "none",
                  height: 40,
                  paddingTop: 10
                }}
                onChange={this.notesToSet}
                defaultValue={defaultValue.notes} />
              <button className={"f9 pointer ml3 ba pa2 pl3 pr3 b--red2 red2 " +
                ((props.contact.notes === "") ? "dn" : "dib")}
                onClick={() => this.setField("removeNotes")}>
                Delete
                </button>
            </div>
            <button className={"pointer db mv2 f9 ba pa2 pl3 pr3 " +
              (((props.contact.notes === this.state.notesToSet)
                || (this.state.notesToSet === ""))
                ? "b--gray4 gray4"
                : "b--black")}
              onClick={() => this.setField("notes")}>
              Save
              </button>

          </div>
        </div>
      </div>
    )
  }

  renderCard() {
    const { props } = this;
    let shipType = this.shipParser(props.ship);
    let currentColor = props.contact.color ? props.contact.color : "0x0";
    let hexColor = uxToHex(currentColor);

    let avatar = 
      (props.contact.avatar !== "TODO") ?
      <img className="dib h-auto" width={128} src={props.contact.avatar} /> :
      <Sigil ship={props.ship} size={128} color={"#" + hexColor} />;

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
                    <p classname="f9 mt6 gray2">website</p>
                    <a target="_blank"
                       classname="bb b--black f8" 
                       href={websiteHref}>
                      {props.contact.website}
                    </a>
                  </div>
                ) : null
              }
              { !!props.contact.notes ? (
                  <div>
                    <p classname="f9 mt6 gray2">notes</p>
                    <p classname="f8">{props.contact.notes}</p>
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

    let ourOpt = (props.ship === window.ship) ? "dib" : "dn";
    let localOpt =
      ((props.ship === window.ship) && (props.path === "/~/default"))
      ? "dib" : "dn";
    let adminOpt =
      (props.path.includes(window.ship) && (props.ship !== window.ship))
      ? "dib" : "dn";

    let card = (state.edit) ? this.renderEditCard() : this.renderCard();

    //TODO "Share card" if it's /me -> sends to /~/default of recipient
    return (
      <div className="h-100 w-100 overflow-x-hidden">
        <div className="w-100 bg-white fixed bb b--gray4">
          <div className="w-100 h2 dn-m dn-l dn-xl inter pb6 pl3 pt3 f8">
            <Link to="/~contacts/">{"⟵"}</Link>
          </div>
          <button 
            onClick={this.editToggle}
            className={`ml3 mt2 mb2 f9 pa1 ba br2 pointer b--black ` + ourOpt}>
            {editInfoText}
          </button>
          <button
            className={`ml3 mt2 mb2 f9 pa1 ba br2 b--black ` + localOpt}>
            Share Contact Info
          </button>
          <button
            className={`ml3 mt2 mb2 f9 pa1 ba red2 br2 b--red2 ` + adminOpt}
            onClick={this.removeContact}>
            Remove from Group
          </button>
        </div>
        {card}
      </div>
    );
  }
}

export default ContactCard;
