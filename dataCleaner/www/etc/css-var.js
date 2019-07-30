import {CSSGlobalVariables} from '../cssVar/css-global-variables.js';
let cssVar = new CSSGlobalVariables();
let barSize = {barLeft: 0, barRight: 'auto'};
const set_bar = (x, y) => {
    barSize.barLeft = x;
    barSize.barRight = y;
    cssVar.barLeft = barSize.barLeft + 'px';
    cssVar.barRight = `calc(100% - ${barSize.barRight}px)`;
};

const change_bar = (el) => {
    let left = $(el).offset().left;
    let right = $(el).offset().left + $(el).outerWidth();

    if (left != barSize.barLeft || right != barSize.barRight) {
        if (left < barSize.barLeft) {
            set_bar(left, barSize.barRight);
        }

        if (right > barSize.barRight){
            set_bar(barSize.barLeft, right);
        }

        setTimeout(set_bar, 150, left, right);
    } 
};

export{cssVar, change_bar, set_bar};