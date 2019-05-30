import React, { PureComponent } from 'react';
import {
  ActivityIndicator,
  StyleSheet,
  View,
  Text,
  Image,
  Linking,
  TouchableOpacity,
  Dimensions
} from 'react-native';
import PropTypes from 'prop-types';
import { fontStyles } from '../utils/styles/index';


export class RenderPost extends Component {
  static propTypes = {
    what: PropTypes.object.isRequired
  };

  parseStyleAttribute(attr) {
    if ('style' in attr) {
      return Object.assign({ flex: 1 }, attr.style);
    }
    return { flex: 1 };
  }

  renderDIV(what, node, attr) {
    let children = what.map((item) => {
      if (typeof(item) === 'string') {
        let style = this.parseStyleAttribute(attr);

        return (
          <Text key={Math.random()} style={[ fontStyles.bodySanFrancisco, style ]}>{item}</Text>
        );
      } else {
        return this.parseContent(item.c, item.gn, item.ga);
      }
    });
    return (
      <View key={Math.random()}>
        { children }
      </View>
    );
  }

  renderP(what, node, attr) {
    let pStyle = fontStyles.bodySanFrancisco;
    let hasSeen = {
      c: false,
      t: false
    };
    let children = what.map((item) => {
      if (typeof(item) === 'string') {
        if (item.trim().length > 0) {
          hasSeen.t = true;
        }

        let style = this.parseStyleAttribute(attr);
        return (
          <Text key={Math.random()} style={[pStyle, style ]}>{item}</Text>
        );
      } else {
        hasSeen.c = true;
        return this.parseContent(item.c, item.gn, Object.assign({style: pStyle}, item.ga));
      }
    });
    let flexDirection = hasSeen.c && hasSeen.t ? 'row' : 'column';
    return (
      <Text key={Math.random()} style={{ flexDirection }}>
        { children }
        {"\n"}
      </Text>
    );
  }

  renderH1(what, node, attr) {
    let h1Style = fontStyles.h1WorkSansSemibold;
    let children = what.map((item) => {
      if (typeof(item) === 'string') {
        let style = this.parseStyleAttribute(attr);
        return (
          <Text key={Math.random()} style={[ style, h1Style ]}>{item}</Text>
        );
      } else {
        return this.parseContent(item.c, item.gn, Object.assign({style: h1Style}, item.ga));
      }
    });
    return (
      <Text key={Math.random()}>
        { children }
        {"\n"}
      </Text>
    );
  }

  renderH2(what, node, attr) {
    let h2Style = fontStyles.h2WorkSansSemibold;
    let children = what.map((item) => {
      if (typeof(item) === 'string') {
        let style = this.parseStyleAttribute(attr);
        return (
          <Text key={Math.random()} style={[ style, h2Style ]}>{item}</Text>
        );
      } else {
        return this.parseContent(item.c, item.gn, Object.assign({style: h2Style}, item.ga));
      }
    });
    return (
      <Text key={Math.random()}>
        { children }
        {"\n"}
      </Text>
    );
  }

  renderH3(what, node, attr) {
    let h3Style = fontStyles.h3WorkSansMedium;
    let children = what.map((item) => {
      if (typeof(item) === 'string') {
        let style = this.parseStyleAttribute(attr);
        return (
          <Text key={Math.random()} style={[ style, h3Style ]}>{item}</Text>
        );
      } else {
        return this.parseContent(item.c, item.gn, Object.assign({style: h3Style}, item.ga));
      }
    });
    return (
      <Text key={Math.random()}>
        { children }
        {"\n"}
      </Text>
    );
  }

  renderH4(what, node, attr) {
    let h4Style = fontStyles.h4SanFranciscoMedium;
    let children = what.map((item) => {
      if (typeof(item) === 'string') {
        let style = this.parseStyleAttribute(attr);
        return (
          <Text key={Math.random()} style={[ style, h4Style ]}>{item}</Text>
        );
      } else {
        return this.parseContent(item.c, item.gn, Object.assign({style: h4Style}, item.ga));
      }
    });
    return (
      <Text key={Math.random()}>
        { children }
        {"\n"}
      </Text>
    );
  }

  renderEM(what, node, attr) {
    let emStyle = { fontWeight: '700' };
    let children = what.map((item) => {
      if (typeof(item) === 'string') {
        let style = this.parseStyleAttribute(attr);
        return (
          <Text key={Math.random()} style={[fontStyles.bodySanFrancisco, style, emStyle ]}>{item}</Text>
        );
      } else {
        return this.parseContent(item.c, item.gn, Object.assign({style: emStyle}, item.ga));
      }
    });
    return (
      <Text key={Math.random()}>
        { children }
      </Text>
    );
  }

  renderI(what, node, attr) {
    let iStyle = { fontStyle: 'italic' };
    let children = what.map((item) => {
      if (typeof(item) === 'string') {
        let style = this.parseStyleAttribute(attr);
        return (
          <Text key={Math.random()} style={[fontStyles.bodySanFrancisco, style, iStyle ]}>{item}</Text>
        );
      } else {
        return this.parseContent(item.c, item.gn, Object.assign({style: iStyle}, item.ga));
      }
    });
    return (
      <Text key={Math.random()}>
        { children }
      </Text>
    );
  }

  renderA(what, node, attr) {
    let aStyle = { textDecorationLine: 'underline' };
    let href = '';
    let child = (<Text> </Text>);
    what.forEach((item) => {
      if (typeof(item) === 'string') {
        let style = this.parseStyleAttribute(attr);
        href = attr.href;
        child = (
          <Text key={Math.random()} style={[fontStyles.bodySanFrancisco, style, aStyle ]}>{item}</Text>
        );
        return null;
      } else {
        return null;
      }
    });
    return (
      <Text onPress={ () => {
          Linking.canOpenURL(href).then(supported => {
            if (supported) {
              Linking.openURL(href);
            }
          });
        }}
        key={Math.random()}>
          { child }
      </Text>
    );

  }

  renderCODE(what, node, attr) {
    let codeStyle = { fontFamily: 'Source Code Pro', backgroundColor: '#f1f1f1' };
    let children = what.map((item) => {
      if (typeof(item) === 'string') {
        let style = this.parseStyleAttribute(attr);
        return (
          <Text key={Math.random()} style={[ fontStyles.bodySanFrancisco, style, codeStyle ]}>{item}</Text>
        );
      } else {
        return this.parseContent(item.c, item.gn, Object.assign({style: codeStyle}, item.ga));
      }
    });
    return (
      <Text key={Math.random()}>
        { children }
      </Text>
    );
  }

  renderOL(what, node, attr) {
    let children = what.map((item, i) => {
      if (typeof(item) === 'string') {
        let style = this.parseStyleAttribute(attr);
        return (
          <Text key={Math.random()} style={[ fontStyles.bodySanFrancisco, style ]}>{item}</Text>
        );
      } else {
        return this.parseContent(item.c, item.gn, Object.assign({ renderOL: true, OLIndex: i + 1 }, item.ga));
      }
    });
    return (
      <Text key={Math.random()}>
        { children }
        {"\n"}
      </Text>
    );
  }

  renderUL(what, node, attr) {
    let children = what.map((item) => {
      if (typeof(item) === 'string') {
        let style = this.parseStyleAttribute(attr);
        return (
          <Text key={Math.random()} style={[ fontStyles.bodySanFrancisco, style ]}>{item}</Text>
        );
      } else {
        return this.parseContent(item.c, item.gn, Object.assign({ renderUL: true }, item.ga));
      }
    });
    return (
      <Text key={Math.random()}>
        { children }
        {"\n"}
      </Text>
    );
  }

  renderLI(what, node, attr) {
    let children = what.map((item) => {
      if (typeof(item) === 'string') {
        let style = this.parseStyleAttribute(attr);
        if ('renderOL' in attr) {
          return (
            <Text key={Math.random()} style={[ fontStyles.bodySanFrancisco, { paddingLeft: 8 }, style ]}>{attr.OLIndex}. {item}</Text>
          );
        }

        return (
          <Text key={Math.random()} style={[ fontStyles.bodySanFrancisco, { paddingLeft: 8 }, style ]}>- {item}</Text>
        );
      } else {
        return this.parseContent(item.c, item.gn, Object.assign(attr, item.ga));
      }
    });
    return (
      <Text key={Math.random()}>
        { children }
        {"\n"}
      </Text>
    );
  }

  renderIMG(what, node, attr) {
    let width = Dimensions.get('window').width;
    let uri = attr.src.replace("http://", "https://");
    return (
      <Image key={attr.src}
        resizeMode={'contain'}
        style={{
          width: width,
          height: 200
        }} source={{
          uri
        }} />
    );  
  }

  parseContent(what, node, attr) {
    switch(node) {
      case "div":
        return this.renderDIV(what, node, attr);
      case "p":
        return this.renderP(what, node, attr);
      case "h1":
        return this.renderH1(what, node, attr);
      case "h2":
        return this.renderH2(what, node, attr);
      case "h3":
        return this.renderH3(what, node, attr);
      case "h4":
        return this.renderH4(what, node, attr);
      case "ol":
        return this.renderOL(what, node, attr);
      case "ul":
        return this.renderUL(what, node, attr);
      case "li":
        return this.renderLI(what, node, attr);
      case "b":
      case "em":
        return this.renderEM(what, node, attr);
      case "code":
        return this.renderCODE(what, node, attr);
      case "img":
        return this.renderIMG(what, node, attr);
      case "i":
        return this.renderI(what, node, attr);
      case "a":
        return this.renderA(what, node, attr);
      default:
        //console.log(what, attr, node);
        return null;
    }
  }

  render() {
    console.log(this.props.what.c);
    if (!('c' in this.props.what)) {
      return (
        <View style={{ flex: 1, marginTop: 40, alignItems: 'center' }}>
          <ActivityIndicator size="large" color="#000" />
        </View>
      );
    }
    return (
      <View style={styles.container}>
        { this.parseContent(this.props.what.c, this.props.what.gn, this.props.what.ga) }
      </View>
    );
  }
}

const styles = StyleSheet.create({
  container: {
    flex: 1
  }
});
