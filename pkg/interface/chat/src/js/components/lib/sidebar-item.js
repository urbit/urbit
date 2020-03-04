import React, { Component } from 'react';
import classnames from 'classnames';
import moment from 'moment';

export class SidebarItem extends Component {

  constructor(props) {
    super(props);
    this.state = {
      timeSinceNewestMessage: this.getTimeSinceNewestMessage()
    };
  }

  componentDidMount() {
    this.updateTimeSinceNewestMessageInterval = setInterval( () => {
      this.setState({timeSinceNewestMessage: this.getTimeSinceNewestMessage()});
    }, 60000);
  }

  componentWillUnmount() {
    if (this.updateTimeSinceNewestMessageInterval) {
      clearInterval(this.updateTimeSinceNewestMessageInterval);
      this.updateTimeSinceNewestMessageInterval = null;
    }
  }

  getTimeSinceNewestMessage() {
    return !!this.props.when ?
      moment.unix(this.props.when / 1000).from(moment.utc())
      : '';
  }

  onClick() {
    const { props } = this;
    props.history.push('/~chat/room' + props.box);
  }

  getLetter(lett) {
    if ('text' in lett) {
      return lett.text;
    } else if ('url' in lett) {
      return lett.url;
    } else if ('code' in lett) {
      return lett.code.expression;
    } else if ('me' in lett) {
      return lett.me;
    } else {
      return '';
    }
  }

  render() {
    const { props, state } = this;

    let unreadElem = !!props.unread
                     ? "green2"
                     : "";

    let title = props.title;

    let box = props.box.substr(1);

    let latest = this.getLetter(props.latest);

    let selectedCss = !!props.selected ? 'bg-gray5 bg-gray1-d gray3-d' : 'bg-white bg-gray0-d gray3-d pointer';

    let authorCss = (props.nickname === props.ship)
      ? "mono" : "";

    let author = (props.nickname === props.ship)
      ? `~${props.ship}` : props.nickname;

    return (
      <div
        className={"z1 pa3 pt2 pb2 bb b--gray4 b--gray1-d " + selectedCss}
        onClick={this.onClick.bind(this)}>
        <div className="w-100 v-mid">
          <p className="dib f8">
              {title}
          </p>
          <p className="f8 db mono gray3 gray2-d pt1">{box}</p>
        </div>
        <div className="w-100 pt3">
          <p className={((unreadElem === "") ? "black white-d" : "") +
          unreadElem + " dib f9 mr3 mw4 truncate v-mid " + authorCss}>
            {(author === "~") ? "" : author}
          </p>
          <p className="dib mono f9 gray3 v-mid">{state.timeSinceNewestMessage}</p>
        </div>
        <p className="f8 clamp-3 pt1">{latest}</p>
      </div>
    );
  }
}
