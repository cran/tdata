// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// ConvertTo_Daily
List ConvertTo_Daily(SEXP w, SEXP aggregateFun);
RcppExport SEXP _tdata_ConvertTo_Daily(SEXP wSEXP, SEXP aggregateFunSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type w(wSEXP);
    Rcpp::traits::input_parameter< SEXP >::type aggregateFun(aggregateFunSEXP);
    rcpp_result_gen = Rcpp::wrap(ConvertTo_Daily(w, aggregateFun));
    return rcpp_result_gen;
END_RCPP
}
// ConvertTo_MultiDaily
List ConvertTo_MultiDaily(SEXP w, int k, SEXP aggregateFun, bool fromEnd);
RcppExport SEXP _tdata_ConvertTo_MultiDaily(SEXP wSEXP, SEXP kSEXP, SEXP aggregateFunSEXP, SEXP fromEndSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type w(wSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< SEXP >::type aggregateFun(aggregateFunSEXP);
    Rcpp::traits::input_parameter< bool >::type fromEnd(fromEndSEXP);
    rcpp_result_gen = Rcpp::wrap(ConvertTo_MultiDaily(w, k, aggregateFun, fromEnd));
    return rcpp_result_gen;
END_RCPP
}
// ConvertTo_Weekly
List ConvertTo_Weekly(SEXP w, const char* weekStart, SEXP aggregateFun);
RcppExport SEXP _tdata_ConvertTo_Weekly(SEXP wSEXP, SEXP weekStartSEXP, SEXP aggregateFunSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type w(wSEXP);
    Rcpp::traits::input_parameter< const char* >::type weekStart(weekStartSEXP);
    Rcpp::traits::input_parameter< SEXP >::type aggregateFun(aggregateFunSEXP);
    rcpp_result_gen = Rcpp::wrap(ConvertTo_Weekly(w, weekStart, aggregateFun));
    return rcpp_result_gen;
END_RCPP
}
// ConvertTo_XxYear
List ConvertTo_XxYear(SEXP w, SEXP aggregateFun, int x);
RcppExport SEXP _tdata_ConvertTo_XxYear(SEXP wSEXP, SEXP aggregateFunSEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type w(wSEXP);
    Rcpp::traits::input_parameter< SEXP >::type aggregateFun(aggregateFunSEXP);
    Rcpp::traits::input_parameter< int >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(ConvertTo_XxYear(w, aggregateFun, x));
    return rcpp_result_gen;
END_RCPP
}
// F_CrossSection
SEXP F_CrossSection(int position);
RcppExport SEXP _tdata_F_CrossSection(SEXP positionSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type position(positionSEXP);
    rcpp_result_gen = Rcpp::wrap(F_CrossSection(position));
    return rcpp_result_gen;
END_RCPP
}
// F_Yearly
SEXP F_Yearly(int year);
RcppExport SEXP _tdata_F_Yearly(SEXP yearSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type year(yearSEXP);
    rcpp_result_gen = Rcpp::wrap(F_Yearly(year));
    return rcpp_result_gen;
END_RCPP
}
// F_Quarterly
SEXP F_Quarterly(int year, int quarter);
RcppExport SEXP _tdata_F_Quarterly(SEXP yearSEXP, SEXP quarterSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type year(yearSEXP);
    Rcpp::traits::input_parameter< int >::type quarter(quarterSEXP);
    rcpp_result_gen = Rcpp::wrap(F_Quarterly(year, quarter));
    return rcpp_result_gen;
END_RCPP
}
// F_Monthly
SEXP F_Monthly(int year, int month);
RcppExport SEXP _tdata_F_Monthly(SEXP yearSEXP, SEXP monthSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type year(yearSEXP);
    Rcpp::traits::input_parameter< int >::type month(monthSEXP);
    rcpp_result_gen = Rcpp::wrap(F_Monthly(year, month));
    return rcpp_result_gen;
END_RCPP
}
// F_MultiYearly
SEXP F_MultiYearly(int year, int z);
RcppExport SEXP _tdata_F_MultiYearly(SEXP yearSEXP, SEXP zSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type year(yearSEXP);
    Rcpp::traits::input_parameter< int >::type z(zSEXP);
    rcpp_result_gen = Rcpp::wrap(F_MultiYearly(year, z));
    return rcpp_result_gen;
END_RCPP
}
// F_XTimesAYear
SEXP F_XTimesAYear(int year, int x, int position);
RcppExport SEXP _tdata_F_XTimesAYear(SEXP yearSEXP, SEXP xSEXP, SEXP positionSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type year(yearSEXP);
    Rcpp::traits::input_parameter< int >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type position(positionSEXP);
    rcpp_result_gen = Rcpp::wrap(F_XTimesAYear(year, x, position));
    return rcpp_result_gen;
END_RCPP
}
// F_XTimesZYears
SEXP F_XTimesZYears(int year, int x, int z, int position);
RcppExport SEXP _tdata_F_XTimesZYears(SEXP yearSEXP, SEXP xSEXP, SEXP zSEXP, SEXP positionSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type year(yearSEXP);
    Rcpp::traits::input_parameter< int >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type z(zSEXP);
    Rcpp::traits::input_parameter< int >::type position(positionSEXP);
    rcpp_result_gen = Rcpp::wrap(F_XTimesZYears(year, x, z, position));
    return rcpp_result_gen;
END_RCPP
}
// F_Weekly
SEXP F_Weekly(int year, int month, int day);
RcppExport SEXP _tdata_F_Weekly(SEXP yearSEXP, SEXP monthSEXP, SEXP daySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type year(yearSEXP);
    Rcpp::traits::input_parameter< int >::type month(monthSEXP);
    Rcpp::traits::input_parameter< int >::type day(daySEXP);
    rcpp_result_gen = Rcpp::wrap(F_Weekly(year, month, day));
    return rcpp_result_gen;
END_RCPP
}
// F_MultiWeekly
SEXP F_MultiWeekly(int year, int month, int day, int k);
RcppExport SEXP _tdata_F_MultiWeekly(SEXP yearSEXP, SEXP monthSEXP, SEXP daySEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type year(yearSEXP);
    Rcpp::traits::input_parameter< int >::type month(monthSEXP);
    Rcpp::traits::input_parameter< int >::type day(daySEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(F_MultiWeekly(year, month, day, k));
    return rcpp_result_gen;
END_RCPP
}
// F_Daily
SEXP F_Daily(int year, int month, int day);
RcppExport SEXP _tdata_F_Daily(SEXP yearSEXP, SEXP monthSEXP, SEXP daySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type year(yearSEXP);
    Rcpp::traits::input_parameter< int >::type month(monthSEXP);
    Rcpp::traits::input_parameter< int >::type day(daySEXP);
    rcpp_result_gen = Rcpp::wrap(F_Daily(year, month, day));
    return rcpp_result_gen;
END_RCPP
}
// F_MultiDaily
SEXP F_MultiDaily(int year, int month, int day, int k);
RcppExport SEXP _tdata_F_MultiDaily(SEXP yearSEXP, SEXP monthSEXP, SEXP daySEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type year(yearSEXP);
    Rcpp::traits::input_parameter< int >::type month(monthSEXP);
    Rcpp::traits::input_parameter< int >::type day(daySEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(F_MultiDaily(year, month, day, k));
    return rcpp_result_gen;
END_RCPP
}
// F_DailyInWeek
SEXP F_DailyInWeek(int year, int month, int day, std::string weekStart, std::string weekEnd, bool forward);
RcppExport SEXP _tdata_F_DailyInWeek(SEXP yearSEXP, SEXP monthSEXP, SEXP daySEXP, SEXP weekStartSEXP, SEXP weekEndSEXP, SEXP forwardSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type year(yearSEXP);
    Rcpp::traits::input_parameter< int >::type month(monthSEXP);
    Rcpp::traits::input_parameter< int >::type day(daySEXP);
    Rcpp::traits::input_parameter< std::string >::type weekStart(weekStartSEXP);
    Rcpp::traits::input_parameter< std::string >::type weekEnd(weekEndSEXP);
    Rcpp::traits::input_parameter< bool >::type forward(forwardSEXP);
    rcpp_result_gen = Rcpp::wrap(F_DailyInWeek(year, month, day, weekStart, weekEnd, forward));
    return rcpp_result_gen;
END_RCPP
}
// F_ListString
SEXP F_ListString(std::vector<std::string> items, std::string value);
RcppExport SEXP _tdata_F_ListString(SEXP itemsSEXP, SEXP valueSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<std::string> >::type items(itemsSEXP);
    Rcpp::traits::input_parameter< std::string >::type value(valueSEXP);
    rcpp_result_gen = Rcpp::wrap(F_ListString(items, value));
    return rcpp_result_gen;
END_RCPP
}
// F_ListDate
SEXP F_ListDate(std::vector<std::string> items, std::string value);
RcppExport SEXP _tdata_F_ListDate(SEXP itemsSEXP, SEXP valueSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<std::string> >::type items(itemsSEXP);
    Rcpp::traits::input_parameter< std::string >::type value(valueSEXP);
    rcpp_result_gen = Rcpp::wrap(F_ListDate(items, value));
    return rcpp_result_gen;
END_RCPP
}
// F_Hourly
SEXP F_Hourly(SEXP day, int hour);
RcppExport SEXP _tdata_F_Hourly(SEXP daySEXP, SEXP hourSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type day(daySEXP);
    Rcpp::traits::input_parameter< int >::type hour(hourSEXP);
    rcpp_result_gen = Rcpp::wrap(F_Hourly(day, hour));
    return rcpp_result_gen;
END_RCPP
}
// F_Minutely
SEXP F_Minutely(SEXP day, int minute);
RcppExport SEXP _tdata_F_Minutely(SEXP daySEXP, SEXP minuteSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type day(daySEXP);
    Rcpp::traits::input_parameter< int >::type minute(minuteSEXP);
    rcpp_result_gen = Rcpp::wrap(F_Minutely(day, minute));
    return rcpp_result_gen;
END_RCPP
}
// F_Secondly
SEXP F_Secondly(SEXP day, int second);
RcppExport SEXP _tdata_F_Secondly(SEXP daySEXP, SEXP secondSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type day(daySEXP);
    Rcpp::traits::input_parameter< int >::type second(secondSEXP);
    rcpp_result_gen = Rcpp::wrap(F_Secondly(day, second));
    return rcpp_result_gen;
END_RCPP
}
// F_XTimesADay
SEXP F_XTimesADay(SEXP day, int x, int position);
RcppExport SEXP _tdata_F_XTimesADay(SEXP daySEXP, SEXP xSEXP, SEXP positionSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type day(daySEXP);
    Rcpp::traits::input_parameter< int >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type position(positionSEXP);
    rcpp_result_gen = Rcpp::wrap(F_XTimesADay(day, x, position));
    return rcpp_result_gen;
END_RCPP
}
// ToString_F
std::string ToString_F(SEXP value);
RcppExport SEXP _tdata_ToString_F(SEXP valueSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type value(valueSEXP);
    rcpp_result_gen = Rcpp::wrap(ToString_F(value));
    return rcpp_result_gen;
END_RCPP
}
// ToClassString_F
std::string ToClassString_F(SEXP value);
RcppExport SEXP _tdata_ToClassString_F(SEXP valueSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type value(valueSEXP);
    rcpp_result_gen = Rcpp::wrap(ToClassString_F(value));
    return rcpp_result_gen;
END_RCPP
}
// ToString_F0
List ToString_F0(SEXP value);
RcppExport SEXP _tdata_ToString_F0(SEXP valueSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type value(valueSEXP);
    rcpp_result_gen = Rcpp::wrap(ToString_F0(value));
    return rcpp_result_gen;
END_RCPP
}
// Parse_F
SEXP Parse_F(std::string str, std::string classStr);
RcppExport SEXP _tdata_Parse_F(SEXP strSEXP, SEXP classStrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type str(strSEXP);
    Rcpp::traits::input_parameter< std::string >::type classStr(classStrSEXP);
    rcpp_result_gen = Rcpp::wrap(Parse_F(str, classStr));
    return rcpp_result_gen;
END_RCPP
}
// Sequence_F0
std::vector<std::string> Sequence_F0(SEXP start, int length, int by);
RcppExport SEXP _tdata_Sequence_F0(SEXP startSEXP, SEXP lengthSEXP, SEXP bySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type start(startSEXP);
    Rcpp::traits::input_parameter< int >::type length(lengthSEXP);
    Rcpp::traits::input_parameter< int >::type by(bySEXP);
    rcpp_result_gen = Rcpp::wrap(Sequence_F0(start, length, by));
    return rcpp_result_gen;
END_RCPP
}
// Sequence_F
std::vector<std::string> Sequence_F(SEXP from, SEXP to, int by);
RcppExport SEXP _tdata_Sequence_F(SEXP fromSEXP, SEXP toSEXP, SEXP bySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type from(fromSEXP);
    Rcpp::traits::input_parameter< SEXP >::type to(toSEXP);
    Rcpp::traits::input_parameter< int >::type by(bySEXP);
    rcpp_result_gen = Rcpp::wrap(Sequence_F(from, to, by));
    return rcpp_result_gen;
END_RCPP
}
// F_GetClass
int F_GetClass(std::string name);
RcppExport SEXP _tdata_F_GetClass(SEXP nameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type name(nameSEXP);
    rcpp_result_gen = Rcpp::wrap(F_GetClass(name));
    return rcpp_result_gen;
END_RCPP
}
// F_Next
SEXP F_Next(SEXP freq, int steps);
RcppExport SEXP _tdata_F_Next(SEXP freqSEXP, SEXP stepsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type freq(freqSEXP);
    Rcpp::traits::input_parameter< int >::type steps(stepsSEXP);
    rcpp_result_gen = Rcpp::wrap(F_Next(freq, steps));
    return rcpp_result_gen;
END_RCPP
}
// F_Minus
int F_Minus(SEXP freq1, SEXP freq2);
RcppExport SEXP _tdata_F_Minus(SEXP freq1SEXP, SEXP freq2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type freq1(freq1SEXP);
    Rcpp::traits::input_parameter< SEXP >::type freq2(freq2SEXP);
    rcpp_result_gen = Rcpp::wrap(F_Minus(freq1, freq2));
    return rcpp_result_gen;
END_RCPP
}
// Get_Descriptive
NumericVector Get_Descriptive(NumericVector w, const char* type, bool skipNAN);
RcppExport SEXP _tdata_Get_Descriptive(SEXP wSEXP, SEXP typeSEXP, SEXP skipNANSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type w(wSEXP);
    Rcpp::traits::input_parameter< const char* >::type type(typeSEXP);
    Rcpp::traits::input_parameter< bool >::type skipNAN(skipNANSEXP);
    rcpp_result_gen = Rcpp::wrap(Get_Descriptive(w, type, skipNAN));
    return rcpp_result_gen;
END_RCPP
}
// Variable
List Variable(SEXP data, SEXP name, SEXP startFrequency, SEXP fields);
RcppExport SEXP _tdata_Variable(SEXP dataSEXP, SEXP nameSEXP, SEXP startFrequencySEXP, SEXP fieldsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    Rcpp::traits::input_parameter< SEXP >::type name(nameSEXP);
    Rcpp::traits::input_parameter< SEXP >::type startFrequency(startFrequencySEXP);
    Rcpp::traits::input_parameter< SEXP >::type fields(fieldsSEXP);
    rcpp_result_gen = Rcpp::wrap(Variable(data, name, startFrequency, fields));
    return rcpp_result_gen;
END_RCPP
}
// VariableToString
std::string VariableToString(List w);
RcppExport SEXP _tdata_VariableToString(SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type w(wSEXP);
    rcpp_result_gen = Rcpp::wrap(VariableToString(w));
    return rcpp_result_gen;
END_RCPP
}
// BindVariables
List BindVariables(SEXP varList, bool interpolate, bool adjustLeadLags, int numExo, int horizon);
RcppExport SEXP _tdata_BindVariables(SEXP varListSEXP, SEXP interpolateSEXP, SEXP adjustLeadLagsSEXP, SEXP numExoSEXP, SEXP horizonSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type varList(varListSEXP);
    Rcpp::traits::input_parameter< bool >::type interpolate(interpolateSEXP);
    Rcpp::traits::input_parameter< bool >::type adjustLeadLags(adjustLeadLagsSEXP);
    Rcpp::traits::input_parameter< int >::type numExo(numExoSEXP);
    Rcpp::traits::input_parameter< int >::type horizon(horizonSEXP);
    rcpp_result_gen = Rcpp::wrap(BindVariables(varList, interpolate, adjustLeadLags, numExo, horizon));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_tdata_ConvertTo_Daily", (DL_FUNC) &_tdata_ConvertTo_Daily, 2},
    {"_tdata_ConvertTo_MultiDaily", (DL_FUNC) &_tdata_ConvertTo_MultiDaily, 4},
    {"_tdata_ConvertTo_Weekly", (DL_FUNC) &_tdata_ConvertTo_Weekly, 3},
    {"_tdata_ConvertTo_XxYear", (DL_FUNC) &_tdata_ConvertTo_XxYear, 3},
    {"_tdata_F_CrossSection", (DL_FUNC) &_tdata_F_CrossSection, 1},
    {"_tdata_F_Yearly", (DL_FUNC) &_tdata_F_Yearly, 1},
    {"_tdata_F_Quarterly", (DL_FUNC) &_tdata_F_Quarterly, 2},
    {"_tdata_F_Monthly", (DL_FUNC) &_tdata_F_Monthly, 2},
    {"_tdata_F_MultiYearly", (DL_FUNC) &_tdata_F_MultiYearly, 2},
    {"_tdata_F_XTimesAYear", (DL_FUNC) &_tdata_F_XTimesAYear, 3},
    {"_tdata_F_XTimesZYears", (DL_FUNC) &_tdata_F_XTimesZYears, 4},
    {"_tdata_F_Weekly", (DL_FUNC) &_tdata_F_Weekly, 3},
    {"_tdata_F_MultiWeekly", (DL_FUNC) &_tdata_F_MultiWeekly, 4},
    {"_tdata_F_Daily", (DL_FUNC) &_tdata_F_Daily, 3},
    {"_tdata_F_MultiDaily", (DL_FUNC) &_tdata_F_MultiDaily, 4},
    {"_tdata_F_DailyInWeek", (DL_FUNC) &_tdata_F_DailyInWeek, 6},
    {"_tdata_F_ListString", (DL_FUNC) &_tdata_F_ListString, 2},
    {"_tdata_F_ListDate", (DL_FUNC) &_tdata_F_ListDate, 2},
    {"_tdata_F_Hourly", (DL_FUNC) &_tdata_F_Hourly, 2},
    {"_tdata_F_Minutely", (DL_FUNC) &_tdata_F_Minutely, 2},
    {"_tdata_F_Secondly", (DL_FUNC) &_tdata_F_Secondly, 2},
    {"_tdata_F_XTimesADay", (DL_FUNC) &_tdata_F_XTimesADay, 3},
    {"_tdata_ToString_F", (DL_FUNC) &_tdata_ToString_F, 1},
    {"_tdata_ToClassString_F", (DL_FUNC) &_tdata_ToClassString_F, 1},
    {"_tdata_ToString_F0", (DL_FUNC) &_tdata_ToString_F0, 1},
    {"_tdata_Parse_F", (DL_FUNC) &_tdata_Parse_F, 2},
    {"_tdata_Sequence_F0", (DL_FUNC) &_tdata_Sequence_F0, 3},
    {"_tdata_Sequence_F", (DL_FUNC) &_tdata_Sequence_F, 3},
    {"_tdata_F_GetClass", (DL_FUNC) &_tdata_F_GetClass, 1},
    {"_tdata_F_Next", (DL_FUNC) &_tdata_F_Next, 2},
    {"_tdata_F_Minus", (DL_FUNC) &_tdata_F_Minus, 2},
    {"_tdata_Get_Descriptive", (DL_FUNC) &_tdata_Get_Descriptive, 3},
    {"_tdata_Variable", (DL_FUNC) &_tdata_Variable, 4},
    {"_tdata_VariableToString", (DL_FUNC) &_tdata_VariableToString, 1},
    {"_tdata_BindVariables", (DL_FUNC) &_tdata_BindVariables, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_tdata(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
