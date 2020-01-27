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
    } else {
      return '';
    }
  }

  render() {
    const { props, state } = this;

    let unreadElem = !!props.unread
                     ? "fw7 green2"
                     : "";

    let title = props.title.substr(1);

    let description = this.getLetter(props.description);

    let selectedCss = !!props.selected ? 'bg-gray5 bg-gray0-d gray3-d' : 'bg-white bg-black-d gray3-d pointer';

    return (
      <div
        className={"z1 pa3 pt4 pb4 bb b--gray4 b--gray2-d " + selectedCss}
        onClick={this.onClick.bind(this)}>
        <div className="w-100 v-mid">
          <p className={"dib mono f8 " + unreadElem }>
            <span className={(unreadElem === "") ? "gray3 gray2-d" : ""}>
              {title.substr(0, title.indexOf("/"))}/
            </span>
            {title.substr(title.indexOf("/") + 1)}
          </p>
        </div>
        <div className="w-100 pt1">
          <p className="dib mono f9 mr3">
            {props.ship === "" ? "" : "~"}
            {props.ship}
          </p>
          <p className="dib mono f9 gray3">{state.timeSinceNewestMessage}</p>
        </div>
        <p className="f8 clamp-3 pt2">{description}</p>
      </div>
    );
  }
}
