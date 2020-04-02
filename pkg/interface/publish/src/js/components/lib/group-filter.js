import React, { Component } from 'react'

export class GroupFilter extends Component {
  constructor(props) {
    super(props);
    this.state = {
      open: false,
      selected: [],
      groups: [],
      searchTerm: "",
      results: []
    }
    this.toggleOpen = this.toggleOpen.bind(this);
    this.handleClickOutside = this.handleClickOutside.bind(this);
    this.groupIndex = this.groupIndex.bind(this);
    this.search = this.search.bind(this);
    this.addGroup = this.addGroup.bind(this);
    this.deleteGroup = this.deleteGroup.bind(this);
  }

  componentDidMount() {
    document.addEventListener('mousedown', this.handleClickOutside);
    this.groupIndex();
    let selected = localStorage.getItem("urbit-selectedGroups");
    if (selected) {
      this.setState({selected: JSON.parse(selected)}, (() => {
        window.api.setSelected(this.state.selected);
      }))
    }
  }

  componentWillUnmount() {
    document.removeEventListener('mousedown', this.handleClickOutside);
  }

  componentDidUpdate(prevProps) {
    if (prevProps !== this.props) {
      this.groupIndex();
    }
  }

  handleClickOutside(evt) {
    if ((this.dropdown && !this.dropdown.contains(evt.target))
    && (this.toggleButton && !this.toggleButton.contains(evt.target))) {
      this.setState({ open: false });
    }
  }

  toggleOpen() {
    this.setState({open: !this.state.open});
  }

  groupIndex() {
    const { props, state } = this;
    let index = [];
    let associations = !!props.associations ? props.associations.contacts : {};
    index = Object.keys(associations).map((each) => {
      let eachGroup = [];
      eachGroup.push(each);
      let name = each;
      if (associations[each].metadata) {
        name = (associations[each].metadata.title !== "")
          ? associations[each].metadata.title : name;
      }
      eachGroup.push(name);
      return eachGroup;
    });
    this.setState({groups: index})
  }

  search(evt) {
    this.setState({searchTerm: evt.target.value});
    let term = evt.target.value.toLowerCase();

    if (term.length < 3) {
      return this.setState({results: []})
    }

    let groupMatches = [];
    groupMatches = this.state.groups.filter(e => {
      return (e[0].includes(term) || e[1].includes(term));
    });
    this.setState({results: groupMatches});
  }

  addGroup(group) {
    let selected = this.state.selected;
    if (!(group in selected)) {
      selected.push(group);
    }
    this.setState({
      searchTerm: "",
      selected: selected,
      results: []
    }, (() => {
        window.api.setSelected(this.state.selected);
        localStorage.setItem("urbit-selectedGroups", JSON.stringify(this.state.selected));
    }))
  }

  deleteGroup(group) {
    let selected = this.state.selected;
    selected = selected.filter(e => {
      return e !== group;
    });
    this.setState({selected: selected}, (() => {
      window.api.setSelected(this.state.selected);
      localStorage.setItem("urbit-selectedGroups", JSON.stringify(this.state.selected));
    }))
  }

  render() {
    const { props, state } = this;

    let currentGroup = "All Groups";

    if (state.selected.length > 0) {
      let titles = state.selected.map((each) => {
        return each[1];
      })
      currentGroup = titles.join(" + ");
    }

    let buttonOpened = (state.open)
      ? "bg-gray5 bg-gray1-d white-d" : "hover-bg-gray5 hover-bg-gray1-d white-d";

    let dropdownClass = (state.open)
      ? "absolute db z-2 bg-white bg-gray0-d white-d ba b--gray3 b--gray1-d"
      : "dn";

    let inviteCount = (props.invites && Object.keys(props.invites).length > 0)
      ? <template className="dib fr">
        <p className="dib bg-green2 bg-gray2-d white fw6 ph1 br1 v-mid" style={{ marginBottom: 2 }}>
          {Object.keys(props.invites).length}
        </p>
        <span className="dib v-mid ml1">
          <img
          className="v-mid"
          src="/~launch/img/Chevron.png"
          style={{ height: 16, width: 16, paddingBottom: 1 }}
          />
        </span>
      </template>
      : <template className="dib fr" style={{paddingTop: 1}}>
        <span className="dib v-top ml1">
          <img className="v-mid"
          src="/~launch/img/Chevron.png"
          style={{ height: 16, width: 16, paddingBottom: 1 }}
          />
        </span>
      </template>;

    let selectedGroups = <div/>
    let searchResults = <div/>

    if (state.results.length > 0) {
      let groupResults = state.results.map((group => {
        return(
          <li
          key={group[0]}
          className="tl list white-d f9 pv2 ph3 pointer hover-bg-gray4 hover-bg-gray1-d inter" onClick={() => this.addGroup(group)}>
            <span className="mix-blend-diff white">{(group[1]) ? group[1] : group[0]}</span>
          </li>
        )
      }))
      searchResults = (
        <div className={"tl absolute bg-white bg-gray0-d white-d pv3 z-1 w-100 ba b--gray4 b--white-d overflow-y-scroll"} style={{maxWidth: "15.67rem", maxHeight: "8rem"}}>
          <p className="f9 tl gray2 ph3 pb2">Groups</p>
          {groupResults}
        </div>
    )
    }

    if (state.selected.length > 0) {
      let allSelected = this.state.selected.map((each) => {
        let name = each[1];
        return(
          <span
            key={each[0]}
            className={"f9 inter black pa2 bg-gray5 bg-gray1-d " +
            "ba b--gray4 b--gray2-d white-d dib mr2 mt2 c-default"}
          >
          {name}
          <span
          className="white-d ml3 mono pointer"
          onClick={e => this.deleteGroup(each)}>
            x
          </span>
          </span>
        )
      })
      selectedGroups = (
        <div className={
          "f9 gray2 bb bl br b--gray3 b--gray2-d bg-gray0-d " +
          "white-d pa3 db w-100 inter bg-gray5 lh-solid tl"
        }>
        {allSelected}
        </div>
      )
    }

    return (
      <div className="ml1 dib">
        <div className={buttonOpened}
        onClick={() => this.toggleOpen()}
        ref={(el) => this.toggleButton = el}>
        <p className="dib f9 pointer pv1 ph2 mw5 truncate v-mid">{currentGroup}</p>
        </div>
        <div className={dropdownClass}
          style={{ maxHeight: "24rem", width: 285 }}
          ref={(el) => { this.dropdown = el }}>
          <p className="tc bb b--gray3 b--gray1-d gray3 pv4 f9">Group Select and Filter</p>
          <a href="/~groups" className="ma4 bg-gray5 bg-gray1-d f9 tl pa1 br1 db no-underline" style={{paddingLeft: "6.5px", paddingRight: "6.5px"}}>Manage all Groups
          {inviteCount}
          </a>
          <p className="pt4 gray3 f9 tl mh4">Filter Groups</p>
          <div className="relative w-100 ph4 pt2 pb4">
            <input className="ba b--gray3 white-d bg-gray0-d inter w-100 f9 pa2" style={{boxSizing: "border-box"}} placeholder="Group name..."
          onChange={this.search}
          value={state.searchTerm}
          />
          {searchResults}
          {selectedGroups}
          </div>
        </div>
      </div>
    )
  }
}

export default GroupFilter;