import { Gitgraph, templateExtend, TemplateName } from "@gitgraph/react";
import React, { Component } from 'react';
import { BrowserRouter, Route } from "react-router-dom";
import _ from 'lodash';

export class Clay extends Component {
  constructor(props) {
    super(props);

    this.clickety = this.clickety.bind(this);
    this.clickCommit = this.clickCommit.bind(this);
    this.submit = this.submit.bind(this);
    this.graph = this.graph.bind(this);
    this.template = templateExtend(TemplateName.Metro, {
      branch: {
        lineWidth: 6,
      },
      commit: {
        spacing: 40,
        dot: {
          size: 10,
        },
        message: {
          displayHash: false,
          displayAuthor: false,
          font: "normal 16pt monospace",
        }
      },
    });
  }

  componentDidMount() {
    api.getCommits();
  }

  clickety() {
    let { commits, gitgraph } = this.props;
    if ( !commits.commits ) return;

    let commitMap = {};

    commits.commits.forEach(commit => {
      commitMap[commit.commitHash] = commit;
    });

    let data = commits.commits.map(com => {
      console.log(com.commitHash,commits.head);
      let ref = [];
      if (com.commitHash in commits.head) {
        ref = ["HEAD", commits.head[com.commitHash]];
      }
      return {
        refs: ref,
        hash: com.commitHash.slice(2),  // lop off 0v for more unique hash
        parents: com.parents.map(par => {return par.slice(2);}),
        onMessageClick: this.clickCommit,
        subject: "commit: " +
                  com.commitHash.slice(-5) +
                  ", content: " +
                  com.contentHash.slice(-5) +
                  ", parents: " +
                  com.parents.map(par => {return par.slice(-5);}),
        author: {
          name: "me",
          email: "me",
          timestamp: 1500000000000,
    } } });
    gitgraph.import(data);
  }

  clickCommit(commit, args) {
    console.log("click", commit);
    let val = commit.refs.slice(-1)[0];
    if (!val) {
      return
    } else if (this.bobDesk.value == "") {
      this.bobDesk.value = val;
    } else {
      this.aliDesk.value = val;
    }
  }

  submit() {
    //TODO hook up
    api.pottery( {
      ali: this.aliDesk.value,
      bob: this.bobDesk.value,
      germ: this.germ.value,
    });
  }

  graph(gitgraph) {
    this.setState({gitgraph: gitgraph});
  }


  render() {

    let textAreaClasses =
      "f7 mono ba bg-gray0-d white-d pa3 mb2 db " +
      "focus-b--black focus-b--white-d b--gray3 b--gray2-d nowrap "

    const inputs = (<>
      <textarea
        ref={ e => { this.bobDesk = e; } }
        className={textAreaClasses}
        placeholder="target desk"
        spellCheck="false"
        rows={1}
      />
      <textarea
        ref={ e => { this.aliDesk = e; } }
        className={textAreaClasses}
        placeholder="source desk"
        spellCheck="false"
        rows={1}
      />
      <select
        ref={ e => { this.germ = e; } }
        className={textAreaClasses}>
        <option value="mate">%mate: conflict if changed same lines</option>
        <option value="meet">%meet: conflict if changed same files</option>
        <option value="meld">%meld: annotate conflicts</option>
        <option value="fine">%fine: fast-forward (requires ancestor)</option>
        <option value="this">%this: use target desk's data</option>
        <option value="that">%that: use source desk's data</option>
        <option value="init">%init: start new desk (danger!)</option>
      </select>
      <button
        className={textAreaClasses}
        onClick={this.submit}>
        Merge!
      </button>
    </>);

    this.clickety();
    return (
      <div className="cf w-100 flex flex-column pa4 ba-m ba-l ba-xl b--gray2 br1 h-100 h-100-minus-40-m h-100-minus-40-l h-100-minus-40-xl f9 white-d">
        <Gitgraph options={{template: this.template}}>{this.graph}</Gitgraph>
      </div>
    );
  }
}