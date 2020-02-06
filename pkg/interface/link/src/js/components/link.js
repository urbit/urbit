import React, { Component } from 'react'
import { LinksTabBar } from './lib/links-tabbar';
import { SidebarSwitcher } from '/components/lib/icons/icon-sidebar-switch.js';
import { api } from '../api';
import { Route, Link } from 'react-router-dom';
import { Sigil } from '/components/lib/icons/sigil';
import { Comments } from './lib/comments';
import { uxToHex } from '../lib/util';
import moment from 'moment'
import { base64urlEncode } from '../lib/util';

export class LinkDetail extends Component {
  constructor(props) {
    super(props);
    this.state = {
      timeSinceLinkPost: this.getTimeSinceLinkPost(props.data),
      comment: "",
      data: props.data
    };

    this.setComment = this.setComment.bind(this);
  }

  updateData(submission) {
    this.setState({
      data: submission,
      timeSinceLinkPost: this.getTimeSinceLinkPost(submission)
    });
  }

  componentDidMount() {
    // if we have no preloaded data, and we aren't expecting it, get it
    if (!this.state.data.title) {
      api.getSubmission(
        this.props.path, this.props.url, this.updateData.bind(this)
      );
    }

    // start the time-since update timer
    this.updateTimeSinceNewestMessageInterval = setInterval( () => {
      this.setState({timeSinceLinkPost: this.getTimeSinceLinkPost(this.state.data)});
    }, 60000);
  }

  componentDidUpdate(prevProps) {
    if (this.props.url !== prevProps.url) {
      this.updateData(this.props.data);
    }
  }

  componentWillUnmount() {
    if (this.updateTimeSinceNewestMessageInterval) {
      clearInterval(this.updateTimeSinceNewestMessageInterval);
      this.updateTimeSinceNewestMessageInterval = null;
    }
  }

  getTimeSinceLinkPost(data) {
    return !!data.time ?
      moment.unix(data.time / 1000).from(moment.utc())
      : '';
  }

  onClickPost() {
    let url = this.props.url || "";

    let request = api.postComment(
      this.props.path,
      url,
      this.state.comment
    );

    if (request) {
      this.setState({comment: ""})
    }
  }

  setComment(event) {
    this.setState({comment: event.target.value});
  }

  render() {
    let props = this.props;
    let popout = (props.popout) ? "/popout" : "";
    let routePath = props.path + "/" + props.page + "/" + props.linkIndex + "/" + base64urlEncode(props.url);

    const data = this.state.data || props.data;
    let ship = data.ship || "zod";
    let title = data.title || "";
    let url = data.url || "";

    let URLparser = new RegExp(/((?:([\w\d\.-]+)\:\/\/?){1}(?:(www)\.?){0,1}(((?:[\w\d-]+\.)*)([\w\d-]+\.[\w\d]+))){1}(?:\:(\d+)){0,1}((\/(?:(?:[^\/\s\?]+\/)*))(?:([^\?\/\s#]+?(?:.[^\?\s]+){0,1}){0,1}(?:\?([^\s#]+)){0,1})){0,1}(?:#([^#\s]+)){0,1}/);

    let hostname = URLparser.exec(url);

    if (hostname) {
      hostname = hostname[4];
    }

    const commentCount = props.comments
      ? props.comments.totalItems
      : data.commentCount || 0;

    let comments = commentCount + " comment" + ((commentCount === 1) ? "" : "s");

    let nickname = !!props.members[ship]
    ? props.members[ship].nickname
    : "";

    let nameClass = nickname ? "inter" : "mono";

    let color = !!props.members[ship]
    ? uxToHex(props.members[ship].color)
    : "000000";

    let activeClasses = (this.state.comment)
    ? "black b--black pointer"
    : "gray2 b--gray2";

    return (
      <div className="h-100 w-100 overflow-hidden flex flex-column">
      <div
      className={`pl3 pt2 flex relative overflow-x-scroll
      overflow-x-auto-l overflow-x-auto-xl flex-shrink-0
      bb bn-m bn-l bn-xl b--gray4`}
      style={{ height: 48 }}>
        <SidebarSwitcher
        sidebarShown={props.sidebarShown}
        popout={props.popout}/>
        <Link
        className="dib f8 fw4 v-top pt2 gray2"
        to={"/~link" + popout + props.path + "/" + props.page}>
        {"<- Collection index"}
        </Link>
        <LinksTabBar
        {...props}
        popout={popout}
        path={routePath}/>
      </div>
      <div className="w-100 mt2 flex justify-center overflow-y-scroll ph4 pb4">
        <div className="w-100 mw7">
          <div className="pb6 flex">
            <Sigil
            ship={"~" + ship}
            size={36}
            color={"#" + color}
            />
            <div className="flex flex-column ml2">
              <a href={url}
              className="w-100 flex"
              target="_blank">
              <p className="f8 truncate">{title}
              <span className="gray2 ml2 flex-shrink-0">{hostname} ↗</span>
              </p>
              </a>
              <div className="w-100 pt1">
                <span className={"f9 pr2 white-d v-mid " + nameClass}>{(nickname)
                  ? nickname
                  : "~" + ship}
                  </span>
                  <span className="f9 inter gray2 pr3 v-mid">
                  {this.state.timeSinceLinkPost}
                  </span>
                  <Link to={"/~link" + props.path + "/" + props.page + "/" + props.linkIndex + "/" + base64urlEncode(props.url)} className="v-top">
                  <span className="f9 inter gray2">
                  {comments}
                  </span>
                  </Link>
                  </div>
                </div>
              </div>
            <div className="relative ba br1 b--gray4 b--gray2-d mt6 mb6">
              <textarea
              className="w-100 bg-gray0-d white-d f8 pa2 pr8"
              style={{
                resize: "none",
                height: 75
              }}
              placeholder="Leave a comment on this link"
              onChange={this.setComment}
              value={this.state.comment}
              />
              <button className={"f8 bg-gray0-d white-d ml2 absolute "
              + activeClasses}
              disabled={!this.state.comment}
              onClick={this.onClickPost.bind(this)}
              style={{
                bottom: 12,
                right: 8
              }}>
              Post
              </button>
              </div>
            <Comments
            path={props.path}
            key={props.path + props.commentPage}
            comments={props.comments}
            commentPage={props.commentPage}
            members={props.members}
            popout={props.popout}
            url={props.url}
            linkPage={props.page}
            linkIndex={props.linkIndex}
            />
            </div>
          </div>
        </div>
      )
    }
  }

  export default LinkDetail;
